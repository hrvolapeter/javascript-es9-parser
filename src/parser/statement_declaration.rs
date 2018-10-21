use crate::lexer::token::Token;
use crate::parser::estree;
use crate::parser::input_wrapper::InputWrapper;
use crate::parser::node;
// Needed because of bug in compiler, which can't star import structs that use derive
use crate::parser::node::{
    ArrayPattern, AssignmentPattern, AssignmentProperty, BlockStatement, Identifier,
    ObjectExpression, ObjectPattern, RestElement, VariableDeclaration, VariableDeclarator,
};
use nom::types::Input;
use nom::types::*;
use nom::IResult;
use nom::*;
use std::any::Any;

named!(pub ScriptBody<Input<InputWrapper>, Vec<estree::ProgramBody>>, do_parse!(
    res: StatementList >>
    (res.into_iter().map(|s| estree::ProgramBody::ProgramStatement(s)).collect())
));

named!(pub StatementList<Input<InputWrapper>, Vec<Box<estree::Statement>>>, do_parse!(
    res: many0!(StatementListItem) >>
    (res.into_iter().flatten().collect())
));

named!(pub StatementListItem<Input<InputWrapper>, Vec<Box<estree::Statement>>>, alt!(
    Statement
));

named!(pub Statement<Input<InputWrapper>, Vec<Box<estree::Statement>>>, alt!(
    BlockStatement |
    VariableStatement |
    EmptyStatement
));

fn EmptyStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Statement>>> {
    let empty = is_token!(tokens, Token::Semicolon)?;
    Ok((empty.0, vec![box node::EmptyStatement {}]))
}

named!(pub BlockStatement<Input<InputWrapper>, Vec<Box<estree::Statement>>>, alt!(
    Block
));

fn Block(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Vec<Box<estree::Statement>>> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    if let Ok((rest, statement)) = StatementList(brackets.0) {
        let brackets = is_token!(rest, Token::RBrace)?;
        return Ok((brackets.0, vec![box BlockStatement { body: statement }]));
    }
    let brackets = is_token!(brackets.0, Token::RBrace)?;
    Ok((brackets.0, vec![box BlockStatement { body: vec![] }]))
}

fn VariableStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Statement>>> {
    let var = take!(tokens, 1)?;
    if (*var.1.inner)[0] != Token::IdentifierName(String::from("var")) {
        return Err(Err::Error(error_position!(var.0, ErrorKind::Custom(1))));
    }
    let declarations =
        separated_nonempty_list!(var.0, is_token!(Token::Comma), VariableDeclaration)?;
    let semicolon = is_token!(declarations.0, Token::Semicolon)?;
    Ok((semicolon.0, declarations.1))
}

named!(pub VariableDeclaration<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    VariableDeclaration0 | VariableDeclaration1
));

fn VariableDeclaration0(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let binding = BindingIdentifier(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            box VariableDeclaration {
                declarations: vec![VariableDeclarator::new(
                    Box::new(Identifier::from(binding.1)) as Box<estree::Pattern>,
                    Some(init.1),
                )],
                kind: estree::VariableDeclarationKind::Var,
            },
        ));
    }
    return Ok((
        binding.0,
        box VariableDeclaration {
            declarations: vec![VariableDeclarator::new(
                Box::new(Identifier::from(binding.1)) as Box<estree::Pattern>,
                None,
            )],
            kind: estree::VariableDeclarationKind::Var,
        },
    ));
}

fn VariableDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let pattern = BindingPattern(tokens)?;
    let init = Initializer(pattern.0)?;

    Ok((
        init.0,
        box VariableDeclaration {
            declarations: vec![VariableDeclarator::new(pattern.1, Some(init.1))],
            kind: estree::VariableDeclarationKind::Var,
        },
    ))
}

fn BindingIdentifier(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Identifier> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, ident.clone().into()));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub BindingPattern<Input<InputWrapper>, Box<estree::Pattern>>, alt!(
    ObjectBindingPattern | ArrayBindingPattern
));

named!(pub ObjectBindingPattern<Input<InputWrapper>, Box<estree::Pattern>>, do_parse!(
    res: alt!(
        ObjectBindingPattern1 |
        ObjectBindingPattern2 |
        ObjectBindingPattern3 |
        ObjectBindingPattern4
    ) >>
    (Box::new(res))
));

fn ObjectBindingPattern1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let brackets = is_token!(brackets.0, Token::RBrace)?;
    Ok((brackets.0, ObjectPattern { properties: vec![] }))
}

fn ObjectBindingPattern2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let rest = BindingRestProperty(brackets.0)?;
    let brackets = is_token!(rest.0, Token::RBrace)?;

    Ok((
        brackets.0,
        ObjectPattern {
            properties: vec![estree::ObjectPatternProperty::RestElement(Box::new(rest.1))],
        },
    ))
}

fn BindingRestProperty(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, RestElement> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let ident = BindingIdentifier(dots.0)?;
    Ok((
        ident.0,
        RestElement {
            argument: Box::new(ident.1),
        },
    ))
}

fn BindingPropertyList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<estree::ObjectPatternProperty>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), BindingProperty)
}

use nom::dbg;
named!(pub BindingProperty<Input<InputWrapper>, estree::ObjectPatternProperty>, alt!(
    BindingProperty2 |
    BindingProperty1
));

fn SingleNameBinding(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Pattern>> {
    let binding = BindingIdentifier(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            box AssignmentPattern {
                left: Box::new(binding.1.clone()),
                right: init.1,
            },
        ));
    }
    Ok((binding.0, box binding.1))
}

fn BindingProperty1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, estree::ObjectPatternProperty> {
    let simple = SingleNameBinding(tokens)?;
    use self::estree::Expression;
    // Need to downcost to concrete type so we can change between traits
    let key = simple.1.downcast_ref::<AssignmentPattern>();
    let key = if key.is_none() {
        &*simple.1
    } else {
        &*key.unwrap().left
    };
    let key = key.downcast_ref::<Identifier>().unwrap().box_clone();

    Ok((
        simple.0,
        estree::ObjectPatternProperty::AssignmentProperty(box AssignmentProperty {
            key,
            value: simple.1,
            shorthand: true,
        }),
    ))
}

fn BindingProperty2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, estree::ObjectPatternProperty> {
    let binding = PropertyName(tokens)?;
    let colon = is_token!(binding.0, Token::Colon)?;
    let init = BindingElement(colon.0)?;
    Ok((
        init.0,
        estree::ObjectPatternProperty::AssignmentProperty(box AssignmentProperty {
            key: binding.1,
            value: init.1,
            shorthand: false,
        }),
    ))
}

named!(pub LiteralPropertyName<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    IdentifierName |
    StringLiteral |
    NumericLiteral
));

fn IdentifierName(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        let ident: Identifier = ident.clone().into();
        return Ok((identifier.0, Box::new(ident)));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn StringLiteral(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let identifier = take!(tokens, 1)?;
    if let Token::StringLiteral(ident) = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, Box::new(node::StringLiteral(ident.clone()))));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn NumericLiteral(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let identifier = take!(tokens, 1)?;
    if let Token::NumericLiteral(ident) = &(*identifier.1.inner)[0] {
        let ident: node::NumberLiteral = node::NumberLiteral(ident.clone());
        return Ok((identifier.0, Box::new(ident)));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn NullLiteral(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let identifier = take!(tokens, 1)?;
    if let Token::LNull = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, Box::new(node::NullLiteral)));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn BooleanLiteral(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let identifier = take!(tokens, 1)?;
    if let Token::BoolLiteral(b) = (*identifier.1.inner)[0] {
        return Ok((identifier.0, Box::new(node::BooleanLiteral(b))));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub PropertyName<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    LiteralPropertyName |
    ComputedPropertyName
));

fn ComputedPropertyName(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let assign = AssignmentExpression(bracket.0)?;
    let bracket = is_token!(assign.0, Token::RSquare)?;
    Ok((bracket.0, assign.1))
}

named!(pub BindingElement<Input<InputWrapper>, Box<estree::Pattern>>, alt!(
    SingleNameBinding |
    BindingElement1
));

fn BindingElement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Pattern>> {
    let binding = BindingPattern(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            box AssignmentPattern {
                left: binding.1,
                right: init.1,
            },
        ));
    }

    Ok(binding)
}

fn ObjectBindingPattern3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let list = BindingPropertyList(brackets.0)?;
    let brackets = is_token!(list.0, Token::RBrace)?;
    Ok((brackets.0, ObjectPattern { properties: list.1 }))
}

fn ObjectBindingPattern4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let list = BindingPropertyList(brackets.0)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let rest = BindingRestProperty(list.0);
    let brackets = if let Ok(rest) = rest {
        is_token!(rest.0, Token::RBrace)?
    } else {
        is_token!(comma.0, Token::RBrace)?
    };
    Ok((brackets.0, ObjectPattern { properties: list.1 }))
}

named!(pub ArrayBindingPattern<Input<InputWrapper>, Box<estree::Pattern>>, do_parse!(
    res: alt!(
        ArrayBindingPattern1 |
        ArrayBindingPattern2 |
        ArrayBindingPattern3
    ) >> (Box::new(res))
));

fn ArrayBindingPattern1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, ArrayPattern> {
    let mut bracket = is_token!(tokens, Token::LSquare)?.0;
    let mut elision = if let Ok((rest, parsed)) = Elision(bracket) {
        bracket = rest;
        parsed
    } else {
        vec![]
    };
    let rest = BindingRestElement(bracket);
    let input = if let Ok((rest, element)) = rest {
        elision.push(Some(element));
        rest
    } else {
        bracket
    };
    let bracket = is_token!(input, Token::RSquare)?;
    Ok((bracket.0, ArrayPattern { elements: elision }))
}

named!(pub BindingRestElement<Input<InputWrapper>, Box<estree::Pattern>>, alt!(
    BindingRestElement1 |
    BindingRestElement2
));

fn BindingRestElement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Pattern>> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    Ok((
        binding.0,
        box RestElement {
            argument: box binding.1,
        },
    ))
}

fn BindingRestElement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Pattern>> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    Ok((
        binding.0,
        box RestElement {
            argument: binding.1,
        },
    ))
}

fn Elision(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<Box<estree::Pattern>>>> {
    let res = many0!(tokens, is_token!(Token::Comma))?;
    Ok((res.0, res.1.into_iter().map(|_| None).collect()))
}

fn ArrayBindingPattern2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, ArrayPattern> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let binding = BindingElementList(bracket.0)?;
    let bracket = is_token!(binding.0, Token::RSquare)?;
    Ok((
        bracket.0,
        ArrayPattern {
            elements: binding.1,
        },
    ))
}

fn BindingElementList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<Box<estree::Pattern>>>> {
    let list = separated_nonempty_list!(tokens, is_token!(Token::Comma), BindingElisionElement)?;
    Ok((list.0, list.1.into_iter().flatten().collect()))
}

fn BindingElisionElement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<Box<estree::Pattern>>>> {
    if let Ok((rest, mut elision)) = Elision(tokens) {
        let binding = BindingElement(rest)?;
        elision.push(Some(binding.1));
        return Ok((binding.0, elision));
    }
    let binding = BindingElement(tokens)?;
    Ok((binding.0, vec![Some(binding.1)]))
}

fn ArrayBindingPattern3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, ArrayPattern> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let mut binding = BindingElementList(bracket.0)?;
    let comma = is_token!(binding.0, Token::Comma)?;
    let elision = if let Ok((rest, ref mut elision)) = Elision(comma.0) {
        binding.1.append(elision);
        rest
    } else {
        comma.0
    };
    let rest = if let Ok((rest, element)) = BindingRestElement(elision) {
        binding.1.push(Some(element));
        rest
    } else {
        elision
    };
    let bracket = is_token!(rest, Token::RSquare)?;
    Ok((
        bracket.0,
        ArrayPattern {
            elements: binding.1,
        },
    ))
}

fn Initializer(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let eq = is_token!(tokens, Token::Assign)?;
    AssignmentExpression(eq.0)
}

named!(pub AssignmentExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    ConditionalExpression
));

named!(pub ConditionalExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    LogicalORExpression
));

named!(pub LogicalORExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    LogicalANDExpression
));

named!(pub LogicalANDExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    BitwiseORExpression
));

named!(pub BitwiseORExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    BitwiseXORExpression
));

named!(pub BitwiseXORExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    BitwiseANDExpression
));

named!(pub BitwiseANDExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    EqualityExpression
));

named!(pub EqualityExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    RelationalExpression
));

named!(pub RelationalExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    ShiftExpression
));

named!(pub ShiftExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    AdditiveExpression
));

named!(pub AdditiveExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    MultiplicativeExpression
));

named!(pub MultiplicativeExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    ExponentiationExpression
));

named!(pub ExponentiationExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    UnaryExpression
));

named!(pub UnaryExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    UpdateExpression
));

named!(pub UpdateExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    LeftHandSideExpression
));

named!(pub LeftHandSideExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    NewExpression
));

named!(pub NewExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    MemberExpression
));

named!(pub MemberExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    PrimaryExpression
));

named!(pub PrimaryExpression<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    IdentifierReference |
    Literal
));

named!(pub IdentifierReference<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    IdentifierName
));

named!(pub Literal<Input<InputWrapper>, Box<estree::Expression>>, alt!(
    NullLiteral |
    BooleanLiteral |
    NumericLiteral |
    StringLiteral
));

#[cfg(test)]
mod test {
    use super::*;
    use crate::lexer::Lexer;
    use crate::parser::estree::*;
    use crate::parser::estree::{
        AssignmentPattern, Identifier, ObjectPattern, VariableDeclaration,
    };
    use crate::parser::Parser;

    #[test]
    fn simple_var() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens("var a;").unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            assert_eq!(var.get_kind(), &estree::VariableDeclarationKind::Var);
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            assert!(var.get_init().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn simple_assignment_number() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens("var a = 1;").unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::NumberLiteral>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn simple_assignment_string() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = "a";"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            assert_eq!(
                var.get_init()
                    .as_ref()
                    .unwrap()
                    .downcast_ref::<node::StringLiteral>()
                    .unwrap()
                    .0,
                "a"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn script_type() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = true;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        assert_eq!(res.get_source_type(), &ProgramSourceType::Script);
    }

    #[test]
    fn var_has_correct_kind() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = true;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            assert_eq!(var.get_kind(), &estree::VariableDeclarationKind::Var);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn simple_assignment_bool() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = true;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            assert_eq!(
                var.get_init()
                    .as_ref()
                    .unwrap()
                    .downcast_ref::<node::BooleanLiteral>()
                    .unwrap()
                    .0,
                true
            );
        } else {
            unreachable!();
        }
    }

    fn simple_assignment_null() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = null;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn simple_assignment_ident() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = b;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];

            let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(id.get_name(), "a");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn var_multiple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a, b, c;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        for res in res.get_body() {
            if let estree::ProgramBody::ProgramStatement(ref var) = res {
                let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
                let ref var = var.get_declarations()[0];

                let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();

                assert!(var.get_init().is_none());
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn var_multiple_with_init() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var a = 1, b = 2;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        for res in res.get_body() {
            if let estree::ProgramBody::ProgramStatement(ref var) = res {
                let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
                let ref var = var.get_declarations()[0];

                let id = var.get_id().downcast_ref::<node::Identifier>().unwrap();

                var.get_init()
                    .as_ref()
                    .unwrap()
                    .downcast_ref::<node::NumberLiteral>()
                    .unwrap();
            } else {
                unreachable!();
            }
        }
    }

    #[test]
    fn object_destruct_assignment_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {a} = b;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            eprintln!("{:?}", var);
            let object = var.get_id().downcast_ref::<node::ObjectPattern>().unwrap();
            if let ObjectPatternProperty::AssignmentProperty(ref assignment) =
                object.get_properties()[0]
            {
                assignment
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
                estree::AssignmentProperty::get_value(&**assignment)
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
                assert_eq!(assignment.get_shorthand(), true);
            } else {
                unreachable!();
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_multiple_prop() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {a, b, c} = d;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let object = var.get_id().downcast_ref::<node::ObjectPattern>().unwrap();
            for prop in object.get_properties() {
                if let ObjectPatternProperty::AssignmentProperty(ref assignment) = prop {
                    assignment
                        .get_key()
                        .downcast_ref::<node::Identifier>()
                        .unwrap();
                    estree::AssignmentProperty::get_value(&**assignment)
                        .downcast_ref::<node::Identifier>()
                        .unwrap();
                    assert_eq!(assignment.get_shorthand(), true);
                } else {
                    unreachable!();
                }
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_rest() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {...rest} = b;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let object = var.get_id().downcast_ref::<node::ObjectPattern>().unwrap();
            if let ObjectPatternProperty::RestElement(ref rest) = object.properties[0] {
                rest.get_argument()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            } else {
                unreachable!();
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_default_value() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {a = true} = b;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let ref object = var
                .get_id()
                .downcast_ref::<node::ObjectPattern>()
                .unwrap()
                .get_properties()[0];
            if let estree::ObjectPatternProperty::AssignmentProperty(ref assignment) = object {
                assert_eq!(
                    assignment
                        .get_key()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                let pattern = estree::AssignmentProperty::get_value(&**assignment)
                    .downcast_ref::<node::AssignmentPattern>()
                    .unwrap();
                assert_eq!(
                    pattern
                        .get_left()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert_eq!(
                    pattern
                        .get_right()
                        .downcast_ref::<node::BooleanLiteral>()
                        .unwrap()
                        .0,
                    true
                );
            } else {
                unreachable!();
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_default_values() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {a = false, a = false} = c;"#)
                .unwrap()
                .1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let object = var
                .get_id()
                .downcast_ref::<node::ObjectPattern>()
                .unwrap()
                .get_properties();
            for ob in object {
                if let estree::ObjectPatternProperty::AssignmentProperty(ref assignment) = ob {
                    assert_eq!(
                        assignment
                            .get_key()
                            .downcast_ref::<node::Identifier>()
                            .unwrap()
                            .get_name(),
                        "a"
                    );
                    let pattern = estree::AssignmentProperty::get_value(&**assignment)
                        .downcast_ref::<node::AssignmentPattern>()
                        .unwrap();
                    assert_eq!(
                        pattern
                            .get_left()
                            .downcast_ref::<node::Identifier>()
                            .unwrap()
                            .get_name(),
                        "a"
                    );
                    assert_eq!(
                        pattern
                            .get_right()
                            .downcast_ref::<node::BooleanLiteral>()
                            .unwrap()
                            .0,
                        false
                    );
                } else {
                    unreachable!();
                }
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_complex_key() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {[a]: a} = c;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let ref property = var
                .get_id()
                .downcast_ref::<node::ObjectPattern>()
                .unwrap()
                .properties[0];
            if let estree::ObjectPatternProperty::AssignmentProperty(assignment) = property {
                assert_eq!(
                    assignment
                        .get_key()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert_eq!(
                    estree::AssignmentProperty::get_value(&**assignment)
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert_eq!(assignment.get_shorthand(), false);
            } else {
                unreachable!();
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_nested_objects() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {a: {b, d}} = c;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let ref property = var
                .get_id()
                .downcast_ref::<node::ObjectPattern>()
                .unwrap()
                .properties[0];
            if let estree::ObjectPatternProperty::AssignmentProperty(assignment) = property {
                assert_eq!(
                    assignment
                        .get_key()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                let inner = estree::AssignmentProperty::get_value(&**assignment)
                    .downcast_ref::<node::ObjectPattern>()
                    .unwrap();
                if let ObjectPatternProperty::AssignmentProperty(ref assignment) =
                    inner.get_properties()[0]
                {
                    assert_eq!(
                        assignment
                            .get_key()
                            .downcast_ref::<node::Identifier>()
                            .unwrap()
                            .get_name(),
                        "b"
                    );
                }
                assert_eq!(assignment.get_shorthand(), false);
            } else {
                unreachable!();
            }

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn object_destruct_assignment_empty() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var {} = c;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let id = var.get_id().downcast_ref::<node::ObjectPattern>().unwrap();

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_empty() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 0);

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_empty_elision() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [,,] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 2);
            assert!(arr.elements[0].is_none());

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_rest() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [,,...rest] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 3);
            let rest = arr.elements[2]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::RestElement>()
                .unwrap();
            let rest_id = rest.argument.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(rest_id.get_name(), "rest");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_element() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [,,a] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 3);
            let id = arr.elements[2]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(id.get_name(), "a");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_elements() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [, ,a, b] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 4);
            let id = arr.elements[2]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(id.get_name(), "a");
            let id = arr.elements[3]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(id.get_name(), "b");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_element_with_rest() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [, ,a,, ...b] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 5);
            let id = arr.elements[2]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(id.get_name(), "a");
            let rest = arr.elements[4]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::RestElement>()
                .unwrap();
            let rest_id = rest.argument.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(rest_id.get_name(), "b");

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn array_destruct_element_elision_end() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"var [, ,a,,,b] = a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let var = var.downcast_ref::<node::VariableDeclaration>().unwrap();
            let ref var = var.get_declarations()[0];
            let arr = var.get_id().downcast_ref::<node::ArrayPattern>().unwrap();
            assert_eq!(arr.elements.len(), 6);
            let id = arr.elements[2]
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(id.get_name(), "a");
            assert!(arr.elements[4].is_none());

            var.get_init()
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn empty_block() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"{}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let block = var.downcast_ref::<node::BlockStatement>().unwrap();
            assert_eq!(block.body.len(), 0);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn simple_block() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"{var a;}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            let block = var.downcast_ref::<node::BlockStatement>().unwrap();
            assert_eq!(block.body.len(), 1);
            block.body[0]
                .downcast_ref::<node::VariableDeclaration>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn empty_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#";"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref var) = res.get_body()[0] {
            var.downcast_ref::<node::EmptyStatement>().unwrap();
        } else {
            unreachable!();
        }
    }
}
