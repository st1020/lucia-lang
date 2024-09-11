//! This module generates AST datatype used by lucia-lang.
//!
//! Specifically, it generates the `SyntaxKind` enum and a number of newtype
//! wrappers around `SyntaxNode` which implement `syntax::AstNode`.

use std::{
    collections::{BTreeSet, HashSet},
    fmt::Write,
    fs,
};

use itertools::Itertools;
use proc_macro2::{Punct, Spacing};
use quote::{format_ident, quote};
use ungrammar::{Grammar, Rule};

use crate::codegen::utils::{
    add_preamble, ensure_file_contents, pluralize, project_root, reformat, to_lower_snake_case,
    to_pascal_case,
};

pub(crate) fn generate(check: bool) {
    let grammar =
        fs::read_to_string(project_root().join("lucia-lang/src/compiler_new/lucia.ungram"))
            .unwrap()
            .parse()
            .unwrap();
    let ast = lower(&grammar);
    let kinds_src = generate_kind_src(&ast, &grammar);

    let syntax_kinds = generate_syntax_kinds(kinds_src);
    let syntax_kinds_file =
        project_root().join("lucia-lang/src/compiler_new/syntax/generated/syntax_kinds.rs");
    ensure_file_contents(syntax_kinds_file.as_path(), &syntax_kinds, check);

    let ast_tokens = generate_tokens(&ast);
    let ast_tokens_file =
        project_root().join("lucia-lang/src/compiler_new/ast/generated/tokens.rs");
    ensure_file_contents(ast_tokens_file.as_path(), &ast_tokens, check);

    let ast_nodes = generate_nodes(kinds_src, &ast);
    let ast_nodes_file = project_root().join("lucia-lang/src/compiler_new/ast/generated/nodes.rs");
    ensure_file_contents(ast_nodes_file.as_path(), &ast_nodes, check);
}

fn generate_tokens(grammar: &AstSrc) -> String {
    let tokens = grammar.tokens.iter().map(|token| {
        let name = format_ident!("{}", token);
        let kind = format_ident!("{}", token);
        quote! {
            #[derive(Debug, Clone, PartialEq, Eq, Hash)]
            pub struct #name {
                pub(crate) syntax: SyntaxToken,
            }
            impl std::fmt::Display for #name {
                fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                    std::fmt::Display::fmt(&self.syntax, f)
                }
            }
            impl AstToken for #name {
                fn can_cast(kind: SyntaxKind) -> bool { kind == #kind }
                fn cast(syntax: SyntaxToken) -> Option<Self> {
                    if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                }
                fn syntax(&self) -> &SyntaxToken { &self.syntax }
            }
        }
    });

    add_preamble(reformat(
        quote! {
            use crate::compiler_new::{
                ast::AstToken,
                syntax::{
                    SyntaxKind::{self, *},
                    SyntaxToken,
                },
            };

            #(#tokens)*
        }
        .to_string(),
    ))
    .replace("#[derive", "\n#[derive")
}

fn generate_nodes(kinds: KindsSrc, grammar: &AstSrc) -> String {
    let (node_defs, node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .map(|node| {
            let name = format_ident!("{}", node.name);
            let kind = format_ident!("{}", node.name);
            let traits = node
                .traits
                .iter()
                .filter(|trait_name| {
                    // Loops have two expressions so this might collide, therefore manual impl it
                    node.name != "ForExpr" && node.name != "WhileExpr"
                        || trait_name.as_str() != "HasLoopBody"
                })
                .map(|trait_name| {
                    let trait_name = format_ident!("{}", trait_name);
                    quote!(impl ast::#trait_name for #name {})
                });

            let methods = node.fields.iter().map(|field| {
                let method_name = format_ident!("{}", field.method_name());
                let ty = field.ty();

                if field.is_many() {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> AstChildren<#ty> {
                            support::children(&self.syntax)
                        }
                    }
                } else if let Some(token_kind) = field.token_kind() {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::token(&self.syntax, #token_kind)
                        }
                    }
                } else {
                    quote! {
                        #[inline]
                        pub fn #method_name(&self) -> Option<#ty> {
                            support::child(&self.syntax)
                        }
                    }
                }
            });
            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }

                    #(#traits)*

                    impl #name {
                        #(#methods)*
                    }
                },
                quote! {
                    impl AstNode for #name {
                        #[inline]
                        fn can_cast(kind: SyntaxKind) -> bool {
                            kind == #kind
                        }
                        #[inline]
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            if Self::can_cast(syntax.kind()) { Some(Self { syntax }) } else { None }
                        }
                        #[inline]
                        fn syntax(&self) -> &SyntaxNode { &self.syntax }
                    }
                },
            )
        })
        .unzip();

    let (enum_defs, enum_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .enums
        .iter()
        .map(|en| {
            let name = format_ident!("{}", en.name);
            let variants = en.variants.iter().map(|var| {
                let var_name = var.name();
                let ty = var.ty();
                quote! { #var_name(#ty) }
            });
            let traits = en.traits.iter().sorted().map(|trait_name| {
                let trait_name = format_ident!("{}", trait_name);
                quote!(impl ast::#trait_name for #name {})
            });
            let kind_matches= en.variants.iter().filter_map(|var| {
                match var {
                    Variant::Token(_) => Some(var.token_kind()),
                    Variant::Node(node) => {
                        if grammar.enums.iter().map(|en| &en.name).contains(node) {
                            None
                        } else {
                            Some(var.token_kind())
                        }
                    }
                }
            });
            let enum_matches = en.variants.iter().filter_map(|var| {
                match var {
                    Variant::Token(_) => None,
                    Variant::Node(node) => {
                        if grammar.enums.iter().map(|en| &en.name).contains(node) {
                            let name = format_ident!("{}", node);
                            Some(quote! { #name::can_cast(kind) })
                        } else {
                            None
                        }
                    }
                }
            });
            let from_syntaxes = en.variants.iter().map(|var| {
                let var_name = var.name();
                let kind = var.token_kind();
                match var {
                    Variant::Token(_) => quote! { #kind => #name::#var_name(syntax) },
                    Variant::Node(node) => {
                        if grammar.enums.iter().map(|en| &en.name).contains(node) {
                            quote! { _ if #var_name::can_cast(syntax.kind()) => #name::#var_name(#var_name::cast(syntax).unwrap()) }
                        } else {
                            quote! { #kind => #name::#var_name(#var_name { syntax }) }
                        }
                    }
                }
            });
            let to_syntaxes = en.variants.iter().map(|var| {
                let var_name = var.name();
                match var {
                    Variant::Token(_) => quote! { #name::#var_name(it) => it },
                    Variant::Node(_) => quote! { #name::#var_name(it) => it.syntax() },
                }
            });
            let from_traits = en.variants.iter().filter_map(|var| match var {
                Variant::Token(_) => None,
                Variant::Node(node) => {
                    let node_name = format_ident!("{}", node);
                    Some(quote! {
                            impl From<#node_name> for #name {
                                #[inline]
                                fn from(node: #node_name) -> #name {
                                    #name::#node_name(node)
                                }
                            }
                    })
                }
            });

            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub enum #name {
                        #(#variants,)*
                    }

                    #(#traits)*
                },
                quote! {
                    impl AstNode for #name {
                        #[inline]
                        fn can_cast(kind: SyntaxKind) -> bool {
                            matches!(kind, #(#kind_matches)|*) #(|| #enum_matches)*
                        }
                        #[inline]
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            let res = match syntax.kind() {
                                #(#from_syntaxes,)*
                                _ => return None,
                            };
                            Some(res)
                        }
                        #[inline]
                        fn syntax(&self) -> &SyntaxNode {
                            match self {
                                #(#to_syntaxes,)*
                            }
                        }
                    }

                    #(#from_traits)*
                },
            )
        })
        .unzip();

    let (any_node_defs, any_node_boilerplate_impls): (Vec<_>, Vec<_>) = grammar
        .nodes
        .iter()
        .flat_map(|node| node.traits.iter().map(move |t| (t, node)))
        .into_group_map()
        .into_iter()
        .sorted_by_key(|(name, _)| *name)
        .map(|(trait_name, nodes)| {
            let name = format_ident!("Any{}", trait_name);
            let trait_name = format_ident!("{}", trait_name);
            let kinds: Vec<_> = nodes
                .iter()
                .map(|name| format_ident!("{}", name.name))
                .collect();
            let nodes = nodes.iter().map(|node| format_ident!("{}", node.name));
            (
                quote! {
                    #[pretty_doc_comment_placeholder_workaround]
                    #[derive(Debug, Clone, PartialEq, Eq, Hash)]
                    pub struct #name {
                        pub(crate) syntax: SyntaxNode,
                    }
                    impl ast::#trait_name for #name {}
                },
                quote! {
                    impl #name {
                        #[inline]
                        pub fn new<T: ast::#trait_name>(node: T) -> #name {
                            #name {
                                syntax: node.syntax().clone()
                            }
                        }
                    }
                    impl AstNode for #name {
                        #[inline]
                        fn can_cast(kind: SyntaxKind) -> bool {
                            matches!(kind, #(#kinds)|*)
                        }
                        #[inline]
                        fn cast(syntax: SyntaxNode) -> Option<Self> {
                            Self::can_cast(syntax.kind()).then_some(#name { syntax })
                        }
                        #[inline]
                        fn syntax(&self) -> &SyntaxNode {
                            &self.syntax
                        }
                    }

                    #(
                        impl From<#nodes> for #name {
                            #[inline]
                            fn from(node: #nodes) -> #name {
                                #name { syntax: node.syntax }
                            }
                        }
                    )*
                },
            )
        })
        .unzip();

    let enum_names = grammar.enums.iter().map(|it| &it.name);
    let node_names = grammar.nodes.iter().map(|it| &it.name);

    let display_impls = enum_names
        .clone()
        .chain(node_names.clone())
        .map(|it| format_ident!("{}", it))
        .map(|name| {
            quote! {
                impl std::fmt::Display for #name {
                    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                        std::fmt::Display::fmt(self.syntax(), f)
                    }
                }
            }
        });

    let defined_names: HashSet<_> = enum_names.chain(node_names.clone()).collect();

    for node in kinds
        .nodes
        .iter()
        .filter(|&name| !defined_names.iter().any(|&it| it == name))
    {
        eprintln!("Warning: node {} not defined in ast source", node);
    }

    let ast = quote! {
        #![allow(non_snake_case)]
        use crate::{
            compiler_new::{
                ast::{self, support, AstChildren, AstNode},
                syntax::{
                    SyntaxKind::{self, *},
                    SyntaxNode, SyntaxToken,
                },
            },
            T,
        };

        #(#node_defs)*
        #(#enum_defs)*
        #(#any_node_defs)*
        #(#node_boilerplate_impls)*
        #(#enum_boilerplate_impls)*
        #(#any_node_boilerplate_impls)*
        #(#display_impls)*
    };

    let ast = ast.to_string().replace("T ! [", "T![");

    let mut res = String::with_capacity(ast.len() * 2);

    let mut docs = grammar
        .nodes
        .iter()
        .map(|it| &it.doc)
        .chain(grammar.enums.iter().map(|it| &it.doc));

    for chunk in ast.split("# [pretty_doc_comment_placeholder_workaround] ") {
        res.push_str(chunk);
        if let Some(doc) = docs.next() {
            write_doc_comment(doc, &mut res);
        }
    }

    let res = add_preamble(reformat(res));
    res.replace("#[derive", "\n#[derive")
}

fn write_doc_comment(contents: &[String], dest: &mut String) {
    for line in contents {
        writeln!(dest, "///{line}").unwrap();
    }
}

fn generate_syntax_kinds(grammar: KindsSrc) -> String {
    let (single_byte_tokens_values, single_byte_tokens): (Vec<_>, Vec<_>) = grammar
        .punct
        .iter()
        .filter(|(token, _name)| token.len() == 1)
        .map(|(token, name)| (token.chars().next().unwrap(), format_ident!("{}", name)))
        .unzip();

    let punctuation_values = grammar.punct.iter().map(|(token, _name)| {
        if "?[]{}()".contains(token) {
            format!("\"{}\"", token).parse().unwrap()
        } else {
            let cs = token.chars().map(|c| Punct::new(c, Spacing::Joint));
            quote! { #(#cs)* }
        }
    });
    let punctuation = grammar
        .punct
        .iter()
        .map(|(_token, name)| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let keywords_values = grammar.keywords;
    let keywords_idents = keywords_values.iter().map(|kw| format_ident!("{}", kw));
    let keywords_names = keywords_values
        .iter()
        .map(|name| format_ident!("{}", to_pascal_case(name)));
    let keywords = keywords_values
        .iter()
        .map(|name| format_ident!("{}Kw", to_pascal_case(name)))
        .collect::<Vec<_>>();

    let literals_values = grammar.literals;
    let literals_idents = literals_values.iter().map(|kw| format_ident!("{}", kw));
    let literals = literals_values
        .iter()
        .map(|name| format_ident!("{}", to_pascal_case(name)))
        .collect::<Vec<_>>();

    let tokens_values = grammar.tokens;
    let tokens_idents = tokens_values.iter().map(|kw| format_ident!("{}", kw));
    let tokens = tokens_values
        .iter()
        .map(|name| format_ident!("{}", to_pascal_case(name)))
        .collect::<Vec<_>>();

    let nodes = grammar
        .nodes
        .iter()
        .map(|name| format_ident!("{}", name))
        .collect::<Vec<_>>();

    let ast = quote! {
        #![allow(bad_style, missing_docs, unreachable_pub)]

        use crate::compiler_new::token::TokenKind;

        /// The kind of syntax node, e.g. `IDENT`, `USE_KW`, or `STRUCT`.
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
        #[repr(u16)]
        pub enum SyntaxKind {
            Error,
            #(#punctuation,)*
            #(#keywords,)*
            #(#literals,)*
            #(#tokens,)*
            #(#nodes,)*

            // Technical kind so that we can cast from u16 safely
            #[doc(hidden)]
            __LAST,
        }
        use self::SyntaxKind::*;

        impl SyntaxKind {
            pub fn is_keyword(self) -> bool {
                matches!(self, #(#keywords)|*)
            }

            pub fn is_punct(self) -> bool {
                matches!(self, #(#punctuation)|*)
            }

            pub fn is_literal(self) -> bool {
                matches!(self, #(#literals)|*)
            }

            pub fn from_keyword(ident: &str) -> Option<SyntaxKind> {
                let kw = match ident {
                    #(#keywords_values => #keywords,)*
                    _ => return None,
                };
                Some(kw)
            }

            pub fn from_char(c: char) -> Option<SyntaxKind> {
                let tok = match c {
                    #(#single_byte_tokens_values => #single_byte_tokens,)*
                    _ => return None,
                };
                Some(tok)
            }
        }

        impl From<TokenKind> for SyntaxKind {
            fn from(value: TokenKind) -> Self {
                match value {
                    #(TokenKind::#tokens => SyntaxKind::#tokens,)*
                    #(TokenKind::#keywords_names => SyntaxKind::#keywords,)*
                    #(TokenKind::#punctuation => SyntaxKind::#punctuation,)*
                    #(TokenKind::#literals => SyntaxKind::#literals,)*
                    _ => Error,
                }
            }
        }

        #[macro_export]
        macro_rules! T {
            #([#punctuation_values] => { $crate::compiler_new::syntax::SyntaxKind::#punctuation };)*
            #([#keywords_idents] => { $crate::compiler_new::syntax::SyntaxKind::#keywords };)*
            #([#literals_idents] => { $crate::compiler_new::syntax::SyntaxKind::#literals };)*
            #([#tokens_idents] => { $crate::compiler_new::syntax::SyntaxKind::#tokens };)*
        }
    };

    add_preamble(reformat(ast.to_string()))
}

#[derive(Default, Debug)]
struct AstSrc {
    tokens: Vec<String>,
    nodes: Vec<AstNodeSrc>,
    enums: Vec<AstEnumSrc>,
}

#[derive(Debug)]
struct AstNodeSrc {
    doc: Vec<String>,
    name: String,
    traits: Vec<String>,
    fields: Vec<Field>,
}

#[derive(Debug, Eq, PartialEq)]
enum Field {
    Token(String),
    Node {
        name: String,
        ty: String,
        cardinality: Cardinality,
    },
}

#[derive(Debug, Eq, PartialEq)]
enum Cardinality {
    Optional,
    Many,
}

#[derive(Debug)]
struct AstEnumSrc {
    doc: Vec<String>,
    name: String,
    traits: Vec<String>,
    variants: Vec<Variant>,
}

#[derive(Debug, Eq, PartialEq)]
enum Variant {
    Token(String),
    Node(String),
}

impl Field {
    fn is_many(&self) -> bool {
        matches!(
            self,
            Field::Node {
                cardinality: Cardinality::Many,
                ..
            }
        )
    }
    fn token_kind(&self) -> Option<proc_macro2::TokenStream> {
        match self {
            Field::Token(token) => {
                let token: proc_macro2::TokenStream = if "?[]{}()".contains(token) {
                    format!("\"{token}\"")
                } else {
                    token.to_owned()
                }
                .parse()
                .unwrap();
                Some(quote! { T![#token] })
            }
            _ => None,
        }
    }
    fn method_name(&self) -> String {
        match self {
            Field::Token(name) => {
                let name = match name.as_str() {
                    ";" => "semicolon",
                    "->" => "thin_arrow",
                    "{" => "l_curly",
                    "}" => "r_curly",
                    "(" => "l_paren",
                    ")" => "r_paren",
                    "[" => "l_brack",
                    "]" => "r_brack",
                    "<" => "l_angle",
                    ">" => "r_angle",
                    "=" => "eq",
                    "!" => "excl",
                    "*" => "star",
                    "&" => "amp",
                    "-" => "minus",
                    "_" => "underscore",
                    "." => "dot",
                    ".." => "dotdot",
                    "..." => "dotdotdot",
                    "..=" => "dotdoteq",
                    "=>" => "fat_arrow",
                    "@" => "at",
                    ":" => "colon",
                    "::" => "coloncolon",
                    "#" => "pound",
                    "?" => "question_mark",
                    "," => "comma",
                    "|" => "pipe",
                    "~" => "tilde",
                    _ => name,
                };
                format!("{name}_token",)
            }
            Field::Node { name, .. } => {
                if name == "type" {
                    String::from("ty")
                } else {
                    name.to_owned()
                }
            }
        }
    }
    fn ty(&self) -> proc_macro2::Ident {
        match self {
            Field::Token(_) => format_ident!("SyntaxToken"),
            Field::Node { ty, .. } => format_ident!("{}", ty),
        }
    }
}

impl Variant {
    fn name(&self) -> proc_macro2::Ident {
        match self {
            Variant::Token(token) => {
                format_ident!(
                    "{}",
                    to_pascal_case(
                        &PUNCT
                            .iter()
                            .find(|(punct, _)| punct == token || &format!("'{punct}'") == token)
                            .map(|(_, name)| (*name).to_owned())
                            .unwrap_or_else(|| clean_token_name(token))
                    )
                )
            }
            Variant::Node(node) => format_ident!("{}", node),
        }
    }
    fn token_kind(&self) -> proc_macro2::TokenStream {
        match self {
            Variant::Token(token) => {
                let token: proc_macro2::TokenStream = if "?[]{}()".contains(token) {
                    format!("\"{token}\"")
                } else {
                    token.to_owned()
                }
                .parse()
                .unwrap();
                quote! { T![#token] }
            }
            Variant::Node(node) => node.parse().unwrap(),
        }
    }
    fn ty(&self) -> proc_macro2::Ident {
        match self {
            Variant::Token(_) => format_ident!("SyntaxNode"),
            Variant::Node(node) => format_ident!("{}", node),
        }
    }
}

fn clean_token_name(name: &str) -> String {
    let cleaned = name.trim_start_matches(['@', '#']);
    if cleaned.is_empty() {
        name.to_owned()
    } else {
        cleaned.to_owned()
    }
}

fn lower(grammar: &Grammar) -> AstSrc {
    let mut res = AstSrc {
        tokens: "Whitespace LineComment BlockComment Str Int Float Ident"
            .split_ascii_whitespace()
            .map(|it| it.to_owned())
            .collect::<Vec<_>>(),
        ..Default::default()
    };

    let nodes = grammar.iter().collect::<Vec<_>>();

    for &node in &nodes {
        let name = grammar[node].name.clone();
        let rule = &grammar[node].rule;
        match lower_enum(grammar, rule) {
            Some(variants) => {
                res.enums.push(AstEnumSrc {
                    doc: Vec::new(),
                    name,
                    traits: Vec::new(),
                    variants,
                });
            }
            None => {
                let mut fields = Vec::new();
                lower_rule(&mut fields, grammar, None, rule);
                res.nodes.push(AstNodeSrc {
                    doc: Vec::new(),
                    name,
                    traits: Vec::new(),
                    fields,
                });
            }
        }
    }

    deduplicate_fields(&mut res);
    extract_enums(&mut res);
    extract_struct_traits(&mut res);
    extract_enum_traits(&mut res);
    res
}

fn lower_enum(grammar: &Grammar, rule: &Rule) -> Option<Vec<Variant>> {
    let alternatives = match rule {
        Rule::Alt(it) => it,
        _ => return None,
    };
    let mut variants = Vec::new();
    for alternative in alternatives {
        match alternative {
            Rule::Node(node) => variants.push(Variant::Node(grammar[*node].name.clone())),
            Rule::Token(token) => {
                variants.push(Variant::Token(clean_token_name(&grammar[*token].name)))
            }
            _ => return None,
        }
    }
    Some(variants)
}

fn lower_rule(acc: &mut Vec<Field>, grammar: &Grammar, label: Option<&String>, rule: &Rule) {
    if lower_separated_list(acc, grammar, label, rule) {
        return;
    }

    match rule {
        Rule::Node(node) => {
            let ty = grammar[*node].name.clone();
            let name = label.cloned().unwrap_or_else(|| to_lower_snake_case(&ty));
            let field = Field::Node {
                name,
                ty,
                cardinality: Cardinality::Optional,
            };
            acc.push(field);
        }
        Rule::Token(token) => {
            assert!(label.is_none());
            let field = Field::Token(clean_token_name(&grammar[*token].name));
            acc.push(field);
        }
        Rule::Rep(inner) => {
            if let Rule::Node(node) = &**inner {
                let ty = grammar[*node].name.clone();
                let name = label
                    .cloned()
                    .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
                let field = Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Many,
                };
                acc.push(field);
                return;
            }
            panic!("unhandled rule: {rule:?}")
        }
        Rule::Labeled { label: l, rule } => {
            assert!(label.is_none());
            lower_rule(acc, grammar, Some(l), rule);
        }
        Rule::Seq(rules) => {
            for rule in rules {
                lower_rule(acc, grammar, label, rule)
            }
        }
        Rule::Alt(_) => panic!("unhandled rule: {rule:?}"),
        Rule::Opt(rule) => lower_rule(acc, grammar, label, rule),
    }
}

// (T (',' T)* ','?)
fn lower_separated_list(
    acc: &mut Vec<Field>,
    grammar: &Grammar,
    label: Option<&String>,
    rule: &Rule,
) -> bool {
    let rule = match rule {
        Rule::Seq(it) => it,
        _ => return false,
    };
    let (node, repeat, trailing_sep) = match rule.as_slice() {
        [Rule::Node(node), Rule::Rep(repeat), Rule::Opt(trailing_sep)] => {
            (node, repeat, Some(trailing_sep))
        }
        [Rule::Node(node), Rule::Rep(repeat)] => (node, repeat, None),
        _ => return false,
    };
    let repeat = match &**repeat {
        Rule::Seq(it) => it,
        _ => return false,
    };
    if !matches!(
        repeat.as_slice(),
        [comma, Rule::Node(n)]
            if trailing_sep.map_or(true, |it| comma == &**it) && n == node
    ) {
        return false;
    }
    let ty = grammar[*node].name.clone();
    let name = label
        .cloned()
        .unwrap_or_else(|| pluralize(&to_lower_snake_case(&ty)));
    let field = Field::Node {
        name,
        ty,
        cardinality: Cardinality::Many,
    };
    acc.push(field);
    true
}

fn deduplicate_fields(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        let mut i = 0;
        'outer: while i < node.fields.len() {
            for j in 0..i {
                let f1 = &node.fields[i];
                let f2 = &node.fields[j];
                if f1 == f2 {
                    node.fields.remove(i);
                    continue 'outer;
                }
            }
            i += 1;
        }
    }
}

fn extract_enums(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        for enm in &ast.enums {
            let mut to_remove = Vec::new();
            for (i, field) in node.fields.iter().enumerate() {
                let ty = field.ty().to_string();
                if enm.variants.iter().any(|it| it.name() == ty) {
                    to_remove.push(i);
                }
            }
            if to_remove.len() == enm.variants.len() {
                node.remove_field(to_remove);
                let ty = enm.name.clone();
                let name = to_lower_snake_case(&ty);
                node.fields.push(Field::Node {
                    name,
                    ty,
                    cardinality: Cardinality::Optional,
                });
            }
        }
    }
}

const TRAITS: &[(&str, &[&str])] = &[
    ("HasAttrs", &["attrs"]),
    ("HasName", &["name"]),
    ("HasLoopBody", &["loop_body"]),
    ("HasArgList", &["arg_list"]),
];

fn extract_struct_traits(ast: &mut AstSrc) {
    for node in &mut ast.nodes {
        for (name, methods) in TRAITS {
            extract_struct_trait(node, name, methods);
        }
    }
}

fn extract_struct_trait(node: &mut AstNodeSrc, trait_name: &str, methods: &[&str]) {
    let mut to_remove = Vec::new();
    for (i, field) in node.fields.iter().enumerate() {
        let method_name = field.method_name();
        if methods.iter().any(|&it| it == method_name) {
            to_remove.push(i);
        }
    }
    if to_remove.len() == methods.len() {
        node.traits.push(trait_name.to_owned());
        node.remove_field(to_remove);
    }
}

fn extract_enum_traits(ast: &mut AstSrc) {
    'outer: for enm in &mut ast.enums {
        let nodes = &ast.nodes;
        if enm
            .variants
            .iter()
            .any(|var| !matches!(var, Variant::Node(_)))
        {
            continue;
        }
        let mut variant_traits = enm
            .variants
            .iter()
            .map(|var| match var {
                Variant::Token(_) => unreachable!(),
                Variant::Node(node) => node.to_owned(),
            })
            .map(|var| nodes.iter().find(|it| it.name == var))
            .map(|node| node.map(|node| node.traits.iter().cloned().collect::<BTreeSet<_>>()));

        let mut enum_traits = match variant_traits.next() {
            Some(Some(it)) => it,
            _ => continue,
        };
        for traits in variant_traits {
            match traits {
                Some(traits) => enum_traits = enum_traits.intersection(&traits).cloned().collect(),
                None => continue 'outer,
            }
        }
        enm.traits = enum_traits.into_iter().collect();
    }
}

impl AstNodeSrc {
    fn remove_field(&mut self, to_remove: Vec<usize>) {
        to_remove.into_iter().rev().for_each(|idx| {
            self.fields.remove(idx);
        });
    }
}

#[derive(Copy, Clone, Debug)]
struct KindsSrc {
    punct: &'static [(&'static str, &'static str)],
    keywords: &'static [&'static str],
    literals: &'static [&'static str],
    tokens: &'static [&'static str],
    nodes: &'static [&'static str],
}

/// The punctuations of the language.
const PUNCT: &[(&str, &str)] = &[
    (",", "Comma"),
    (".", "Dot"),
    ("(", "OpenParen"),
    (")", "CloseParen"),
    ("{", "OpenBrace"),
    ("}", "CloseBrace"),
    ("[", "OpenBracket"),
    ("]", "CloseBracket"),
    ("#", "Pound"),
    ("?", "Question"),
    ("!", "Exclamation"),
    (":", "Colon"),
    ("=", "Assign"),
    ("<", "Lt"),
    (">", "Gt"),
    ("|", "VBar"),
    ("+", "Add"),
    ("-", "Sub"),
    ("*", "Mul"),
    ("/", "Div"),
    ("%", "Rem"),
    ("::", "DoubleColon"),
    ("?.", "SafeDot"),
    ("?[", "SafeOpenBracket"),
    ("->", "Arrow"),
    ("==", "Eq"),
    ("!=", "NotEq"),
    ("<=", "LtEq"),
    (">=", "GtEq"),
    ("+=", "AddAssign"),
    ("-=", "SubAssign"),
    ("*=", "MulAssign"),
    ("/=", "DivAssign"),
    ("%=", "RemAssign"),
];
const TOKENS: &[&str] = &["line_comment", "block_comment", "whitespace", "eol", "eof"];
const RESERVED: &[&str] = &[];

fn generate_kind_src(ast: &AstSrc, grammar: &ungrammar::Grammar) -> KindsSrc {
    let mut keywords: Vec<&_> = Vec::new();
    let mut tokens: Vec<&_> = TOKENS.to_vec();
    let mut literals: Vec<&_> = Vec::new();
    let mut used_puncts = vec![false; PUNCT.len()];
    // Mark $ as used
    used_puncts[0] = true;
    grammar.tokens().for_each(|token| {
        let name = &*grammar[token].name;
        match name.split_at(1) {
            ("@", lit) if !lit.is_empty() => {
                literals.push(String::leak(lit.to_owned()));
            }
            ("#", token) if !token.is_empty() => {
                tokens.push(String::leak(token.to_owned()));
            }
            _ if name.chars().all(char::is_alphabetic) => {
                keywords.push(String::leak(name.to_owned()));
            }
            _ => {
                let idx = PUNCT
                    .iter()
                    .position(|(punct, _)| punct == &name)
                    .unwrap_or_else(|| panic!("Grammar references unknown punctuation {name:?}"));
                used_puncts[idx] = true;
            }
        }
    });
    PUNCT
        .iter()
        .zip(used_puncts)
        .filter(|(_, used)| !used)
        .for_each(|((punct, _), _)| {
            panic!("Punctuation {punct:?} is not used in grammar");
        });
    keywords.extend(RESERVED.iter().copied());
    keywords.sort();
    keywords.dedup();

    // we leak things here for simplicity, that way we don't have to deal with lifetimes
    // The execution is a one shot job so thats fine
    let nodes = ast
        .nodes
        .iter()
        .map(|it| &it.name)
        // .chain(enums.iter().map(|it| &it.name))
        .cloned()
        .map(String::leak)
        .map(|it| &*it)
        .collect();
    let nodes = Vec::leak(nodes);
    nodes.sort();
    let keywords = Vec::leak(keywords);
    let literals = Vec::leak(literals);
    literals.sort();
    let tokens = Vec::leak(tokens);
    tokens.sort();

    KindsSrc {
        punct: PUNCT,
        nodes,
        keywords,
        literals,
        tokens,
    }
}
