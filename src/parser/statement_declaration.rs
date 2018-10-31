use crate::{
    lexer::token::Token,
    parser::{estree, input_wrapper::InputWrapper, node},
};
// Needed because of bug in compiler, which can't star import structs that use
// derive
use crate::parser::node::{
    ArrayPattern, ArrowFunctionExpression, AssignmentPattern, AssignmentProperty, BlockStatement,
    Identifier, ObjectExpression, ObjectPattern, RestElement, VariableDeclaration,
    VariableDeclarator,
};
use nom::{
    types::{Input, *},
    IResult, *,
};
use std::any::Any;

named!(pub ScriptBody<Input<InputWrapper>, Vec<estree::ProgramBody>>, do_parse!(
    res: StatementList >>
    (res.into_iter().map(|s| estree::ProgramBody::ProgramStatement(s)).collect())
));

named!(pub StatementList<Input<InputWrapper>, Vec<Box<estree::Statement>>>, alt!(
    many0!(StatementListItem)
));

named!(pub StatementListItem<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    Statement |
    do_parse!(
        res: Declaration >>
        (estree::Statement::box_clone(&*res)))
));

named!(pub Declaration<Input<InputWrapper>, Box<estree::Declaration>>, alt!(
    HoistableDeclaration |
    ClassDeclaration |
    do_parse!(
        res: LexicalDeclaration >>
        (estree::Declaration::box_clone(&*res))
    )
));

named!(pub ClassDeclaration<Input<InputWrapper>, Box<estree::Declaration>>, alt!(
    ClassDeclaration1 |
    ClassDeclaration2
));

fn ClassDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Declaration>> {
    let class = is_token!(tokens, Token::KClass)?;
    let ident = BindingIdentifier(class.0)?;
    let herritage = ClassHeritage(ident.0);
    let bracket = is_token!(either(&herritage, ident.0), Token::LBrace)?;
    let body = ClassBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        box node::ClassDeclaration {
            id: Some(box ident.1),
            super_class: unwrap_or_none(herritage),
            body: body.1,
        },
    ))
}

fn ClassDeclaration2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Declaration>> {
    let class = is_token!(tokens, Token::KClass)?;
    let herritage = ClassHeritage(class.0);
    let bracket = is_token!(either(&herritage, class.0), Token::LBrace)?;
    let body = ClassBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    unimplemented!("Annonymous classes are not supported")
}

fn ClassBody(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Box<estree::ClassBody>> {
    let elems = many0!(tokens, ClassElement)?;

    Ok((elems.0, box node::ClassBody { body: elems.1 }))
}

named!(pub ClassElement<Input<InputWrapper>, Box<estree::MethodDefinition>>, alt!(
    ClassElement1 |
    ClassElement2
));

fn ClassElement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::MethodDefinition>> {
    let def = MethodDefinition(tokens)?;
    let semi = many0!(def.0, is_token!(Token::Semicolon))?;

    Ok((semi.0, def.1))
}

fn ClassElement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::MethodDefinition>> {
    let static_ = is_token!(tokens, Token::KStatic)?;
    let mut def = MethodDefinition(static_.0)?;
    def.1
        .downcast_mut::<node::MethodDefinition>()
        .unwrap()
        .static_ = true;
    let semi = many0!(def.0, is_token!(Token::Semicolon))?;

    Ok((semi.0, def.1))
}

named!(pub MethodDefinition<Input<InputWrapper>, Box<estree::MethodDefinition>>, alt!(
    MethodDefinition1 |
    /*GeneratorMethod |
    AsyncMethod |
    AsyncGeneratorMethod |*/
    MethodDefinition5 |
    MethodDefinition6
));

fn MethodDefinition1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::MethodDefinition>> {
    let name = PropertyName(tokens)?;
    let bracket = is_token!(name.0, Token::LRound)?;
    let params = UniqueFormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        box node::MethodDefinition {
            key: name.1,
            value: box node::FunctionExpression {
                id: None,
                params: params.1,
                body: box node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| estree::FunctionBodyEnum::Statement(i))
                        .collect(),
                },
                generator: false,
                async_: false,
            },
            computed: false,
            static_: false,
            kind: estree::MethodDefinitionKind::Method,
        },
    ))
}

fn MethodDefinition5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::MethodDefinition>> {
    let get = is_token!(tokens, Token::KGet)?;
    let name = PropertyName(get.0)?;
    let bracket = is_token!(name.0, Token::LRound)?;
    let bracket = is_token!(bracket.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        box node::MethodDefinition {
            key: name.1,
            value: box node::FunctionExpression {
                id: None,
                params: vec![],
                body: box node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| estree::FunctionBodyEnum::Statement(i))
                        .collect(),
                },
                generator: false,
                async_: false,
            },
            computed: false,
            static_: false,
            kind: estree::MethodDefinitionKind::Get,
        },
    ))
}

fn MethodDefinition6(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::MethodDefinition>> {
    let set = is_token!(tokens, Token::KSet)?;
    let name = PropertyName(set.0)?;
    let bracket = is_token!(name.0, Token::LRound)?;
    let param = FormalParameter(bracket.0)?;
    let bracket = is_token!(param.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        box node::MethodDefinition {
            key: name.1,
            value: box node::FunctionExpression {
                id: None,
                params: vec![param.1],
                body: box node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| estree::FunctionBodyEnum::Statement(i))
                        .collect(),
                },
                generator: false,
                async_: false,
            },
            computed: false,
            static_: false,
            kind: estree::MethodDefinitionKind::Set,
        },
    ))
}

fn UniqueFormalParameters(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    FormalParameters(tokens)
}

fn ClassHeritage(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let extend = is_token!(tokens, Token::KExtend)?;
    LeftHandSideExpression(extend.0)
}

named!(pub HoistableDeclaration<Input<InputWrapper>, Box<estree::Declaration>>, alt!(
    do_parse!(
        res: FunctionDeclaration >>
        (res as Box<estree::Declaration>)) /*|
    GeneratorDeclaration |
    AsyncFunctionDeclaration |
    AsyncGeneratorDeclaration*/
));

named!(pub FunctionDeclaration<Input<InputWrapper>, Box<node::FunctionDeclaration>>, alt!(
    FunctionDeclaration1 |
    FunctionDeclaration2
));

fn FunctionDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<node::FunctionDeclaration>> {
    let function = is_token!(tokens, Token::KFunction)?;
    let ident = BindingIdentifier(function.0)?;
    let bracket = is_token!(ident.0, Token::LRound)?;
    let params = FormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        box node::FunctionDeclaration {
            id: Some(box ident.1),
            params: params.1,
            body: box node::FunctionBody {
                body: body
                    .1
                    .into_iter()
                    .map(|i| estree::FunctionBodyEnum::Statement(i))
                    .collect(),
            },
            generator: false,
            async_: false,
        },
    ))
}

fn FunctionDeclaration2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<node::FunctionDeclaration>> {
    let function = is_token!(tokens, Token::KFunction)?;
    let bracket = is_token!(function.0, Token::LRound)?;
    let params = FormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    unimplemented!("Annonymous functions are not supported")
}

named!(pub FormalParameters<Input<InputWrapper>, Vec<Box<estree::Pattern>>>, alt!(
    FunctionRestParameter |
    FormalParameterList |
    FormalParameters4 |
    FormalParameters5 |
    FormalParameters1
));

fn FormalParameters1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    Ok((tokens, vec![]))
}

fn FunctionRestParameter(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let rest = BindingRestElement(tokens)?;
    Ok((rest.0, vec![rest.1]))
}

fn FormalParameterList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), FormalParameter)
}

fn FormalParameters4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let list = FormalParameterList(tokens)?;
    let comma = is_token!(list.0, Token::Comma)?;

    Ok((comma.0, list.1))
}

fn FormalParameters5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let mut list = FormalParameterList(tokens)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let rest = BindingRestElement(tokens)?;
    list.1.push(rest.1);

    Ok((rest.0, list.1))
}

fn FormalParameter(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Pattern>> {
    BindingElement(tokens)
}
named!(pub Statement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    BlockStatement |
    VariableStatement |
    EmptyStatement |
    ExpressionStatement |
    IfStatement |
    BrekableStatement |
    ContinueStatement |
    BreakStatement |
    ReturnStatement |
    WithStatement |
    LabeledStatement |
    ThrowStatement |
    TryStatement |
    DebuggerStatement
));

fn DebuggerStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let debugger = is_token!(tokens, Token::KDebugger)?;
    let semicolon = is_token!(debugger.0, Token::Semicolon)?;
    Ok((semicolon.0, box node::DebuggerStatement {}))
}

named!(pub TryStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    TryStatement3 |
    TryStatement2 |
    TryStatement1
));

fn TryStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let catch = Catch(block.0)?;
    Ok((
        catch.0,
        box node::TryStatement {
            block: block.1.downcast::<node::BlockStatement>().unwrap(),
            handler: Some(catch.1),
            finalizer: None,
        },
    ))
}

fn TryStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let finally = Finally(block.0)?;
    Ok((
        finally.0,
        box node::TryStatement {
            block: block.1.downcast::<node::BlockStatement>().unwrap(),
            handler: None,
            finalizer: Some(finally.1.downcast::<node::BlockStatement>().unwrap()),
        },
    ))
}

fn TryStatement3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let catch = Catch(block.0)?;
    let finally = Finally(catch.0)?;
    Ok((
        finally.0,
        box node::TryStatement {
            block: block.1.downcast::<node::BlockStatement>().unwrap(),
            handler: Some(catch.1),
            finalizer: Some(finally.1.downcast::<node::BlockStatement>().unwrap()),
        },
    ))
}

fn Finally(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let finally = is_token!(tokens, Token::KFinally)?;
    Block(finally.0)
}

fn Catch(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Box<estree::CatchClause>> {
    let catch = is_token!(tokens, Token::KCatch)?;
    let bracket = is_token!(catch.0, Token::LRound)?;
    let param = CatchParameter(bracket.0)?;
    let bracket = is_token!(param.0, Token::RRound)?;
    let block = Block(bracket.0)?;
    Ok((
        block.0,
        box node::CatchClause {
            param: param.1,
            body: block.1.downcast::<node::BlockStatement>().unwrap(),
        },
    ))
}

named!(pub CatchParameter<Input<InputWrapper>, Box<estree::Pattern>>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (box res as Box<estree::Pattern>)) |
    BindingPattern
));

fn ThrowStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let throw = is_token!(tokens, Token::KThrow)?;
    let expr = Expression(throw.0)?;
    let semi = is_token!(expr.0, Token::Semicolon)?;
    Ok((semi.0, box node::ThrowStatement { argument: expr.1 }))
}

fn LabeledStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let ident = LabelIdentifier(tokens)?;
    let colon = is_token!(ident.0, Token::Colon)?;
    let item = LabeledItem(colon.0)?;
    Ok((
        item.0,
        box node::LabeledStatement {
            label: box ident.1,
            body: item.1,
        },
    ))
}

named!(pub LabeledItem<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    Statement |
    do_parse!(
        res: FunctionDeclaration >>
        (res as Box<estree::Statement>))
));

fn WithStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let with = is_token!(tokens, Token::KWith)?;
    let bracket = is_token!(with.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let stmt = Statement(bracket.0)?;
    Ok((
        stmt.0,
        box node::WithStatement {
            object: expr.1,
            body: stmt.1,
        },
    ))
}

named!(pub ReturnStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    ReturnStatement1 |
    ReturnStatement2
));

fn ReturnStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KReturn)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((semi.0, box node::ReturnStatement { argument: None }))
}

fn ReturnStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KReturn)?;
    let expr = Expression(con.0)?;
    let semi = is_token!(expr.0, Token::Semicolon)?;

    Ok((
        semi.0,
        box node::ReturnStatement {
            argument: Some(expr.1),
        },
    ))
}

named!(pub BreakStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    BreakStatement1 |
    BreakStatement2
));

fn BreakStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KBreak)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((semi.0, box node::BreakStatement { label: None }))
}

fn BreakStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KBreak)?;
    let label = LabelIdentifier(con.0)?;
    let semi = is_token!(label.0, Token::Semicolon)?;

    Ok((
        semi.0,
        box node::BreakStatement {
            label: Some(box label.1),
        },
    ))
}

named!(pub ContinueStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    ContinueStatement1 |
    ContinueStatement2
));

fn ContinueStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KContinue)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((semi.0, box node::ContinueStatement { label: None }))
}

fn ContinueStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let con = is_token!(tokens, Token::KContinue)?;
    let label = LabelIdentifier(con.0)?;
    let semi = is_token!(label.0, Token::Semicolon)?;

    Ok((
        semi.0,
        box node::ContinueStatement {
            label: Some(box label.1),
        },
    ))
}

fn LabelIdentifier(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Identifier> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, ident.clone().into()));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub BrekableStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    IterationStatement |
    SwitchStatement
));

fn SwitchStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let switch_ = is_token!(tokens, Token::KSwitch)?;
    let bracket = is_token!(switch_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let case = CaseBlock(bracket.0)?;
    Ok((
        case.0,
        box node::SwitchStatement {
            discriminant: expr.1,
            case: case.1,
        },
    ))
}

named!(pub CaseBlock<Input<InputWrapper>, Vec<Box<estree::SwitchCase>>>, alt!(
    CaseBlock1 |
    CaseBlock2
));

fn CaseBlock1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::SwitchCase>>> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let clauses = many0!(bracket.0, CaseClause);
    if let Ok((rest, consumed)) = clauses {
        return Ok((rest, consumed));
    }
    let bracket = is_token!(bracket.0, Token::RBrace)?;
    Ok((bracket.0, vec![]))
}

fn CaseBlock2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::SwitchCase>>> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let mut input = bracket.0;
    let mut res = vec![];
    let mut clauses = many0!(bracket.0, CaseClause);
    if let Ok((rest, mut consumed)) = clauses {
        input = rest;
        res.append(&mut consumed);
    }
    let default = DefaultClause(input)?;
    input = default.0;
    res.push(default.1);
    let clauses = many0!(bracket.0, CaseClause);
    if let Ok((rest, mut consumed)) = clauses {
        input = rest;
        res.append(&mut consumed);
    }
    Ok((input, res))
}

fn DefaultClause(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::SwitchCase>> {
    let default = is_token!(tokens, Token::KDefault)?;
    let colon = is_token!(default.0, Token::Colon)?;
    let statements = StatementList(colon.0);

    if let Ok((rest, consumed)) = statements {
        return Ok((
            rest,
            box node::SwitchCase {
                test: None,
                consequent: consumed,
            },
        ));
    }

    Ok((
        colon.0,
        box node::SwitchCase {
            test: None,
            consequent: vec![],
        },
    ))
}

fn CaseClause(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::SwitchCase>> {
    let case = is_token!(tokens, Token::KCase)?;
    let expr = Expression(case.0)?;
    let colon = is_token!(expr.0, Token::Colon)?;
    let statements = StatementList(colon.0);

    if let Ok((rest, consumed)) = statements {
        return Ok((
            rest,
            box node::SwitchCase {
                test: Some(expr.1),
                consequent: consumed,
            },
        ));
    }

    Ok((
        colon.0,
        box node::SwitchCase {
            test: Some(expr.1),
            consequent: vec![],
        },
    ))
}

named!(pub IterationStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    IterationStatement1 |
    IterationStatement2 |
    IterationStatement3 |
    IterationStatement4 |
    IterationStatement5 |
    IterationStatement6 |
    IterationStatement7 |
    IterationStatement8 |
    IterationStatement9 |
    IterationStatement10 |
    IterationStatement11 |
    IterationStatement12 |
    IterationStatement13 |
    IterationStatement14
));

fn IterationStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let do_ = is_token!(tokens, Token::KDo)?;
    let stmt = Statement(do_.0)?;
    let while_ = is_token!(stmt.0, Token::KWhile)?;
    let bracket = is_token!(while_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let semi = is_token!(bracket.0, Token::Semicolon)?;
    Ok((
        semi.0,
        box node::DoWhileStatement {
            body: stmt.1,
            test: expr.1,
        },
    ))
}

fn IterationStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let while_ = is_token!(tokens, Token::KWhile)?;
    let bracket = is_token!(while_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let stmt = Statement(bracket.0)?;

    Ok((
        stmt.0,
        box node::WhileStatement {
            body: stmt.1,
            test: expr.1,
        },
    ))
}

fn IterationStatement3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    not!(
        bracket.0,
        do_parse!(is_token!(Token::KLet) >> is_token!(Token::LSquare) >> (()))
    )?;
    let expr1 = Expression(bracket.0);
    let semicolon = is_token!(either(&expr1, bracket.0), Token::Semicolon)?;
    let expr2 = Expression(semicolon.0);
    let semicolon = is_token!(either(&expr2, semicolon.0), Token::Semicolon)?;
    let expr3 = Expression(semicolon.0);
    let bracket = is_token!(either(&expr3, semicolon.0), Token::RRound)?;
    let stmt = Statement(bracket.0)?;

    Ok((
        stmt.0,
        box node::ForStatement {
            init: unwrap_or_none(expr1).map(|i| estree::ForStatementInit::Expression(i)),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: stmt.1,
        },
    ))
}

fn IterationStatement4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let var = is_token!(bracket.0, Token::KVar)?;
    let declarations =
        separated_nonempty_list!(var.0, is_token!(Token::Comma), VariableDeclaration)?;
    let semicolon = is_token!(declarations.0, Token::Semicolon)?;
    let expr2 = Expression(semicolon.0);
    let semicolon = is_token!(either(&expr2, semicolon.0), Token::Semicolon)?;
    let expr3 = Expression(semicolon.0);
    let bracket = is_token!(either(&expr3, semicolon.0), Token::RRound)?;
    let stmt = Statement(bracket.0)?;

    Ok((
        stmt.0,
        box node::ForStatement {
            init: Some(estree::ForStatementInit::VariableDeclaration(
                box VariableDeclaration {
                    declarations: declarations.1,
                    kind: estree::VariableDeclarationKind::Var,
                },
            )),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: stmt.1,
        },
    ))
}

fn IterationStatement5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let lex = LexicalDeclaration(bracket.0)?;
    let expr2 = Expression(lex.0);
    let semicolon = is_token!(lex.0, Token::Semicolon)?;
    let expr3 = Expression(semicolon.0);
    let bracket = is_token!(either(&expr3, semicolon.0), Token::RRound)?;
    let stmt = Statement(bracket.0)?;

    Ok((
        stmt.0,
        box node::ForStatement {
            init: Some(estree::ForStatementInit::VariableDeclaration(lex.1)),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: stmt.1,
        },
    ))
}

fn IterationStatement6(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    not!(
        bracket.0,
        do_parse!(is_token!(Token::KLet) >> is_token!(Token::LSquare) >> (()))
    )?;
    let left = LeftHandSideExpression(bracket.0)?;
    let in_ = is_token!(left.0, Token::KIn)?;
    let right = Expression(in_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForInStatement {
            left: estree::ForInStatementLeft::Pattern(ExpressionToPattern(left.1)),
            right: right.1,
            body: body.1,
        },
    ))
}

fn IterationStatement7(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let var = is_token!(bracket.0, Token::KVar)?;
    let biding = ForBinding(var.0)?;
    let in_ = is_token!(biding.0, Token::KIn)?;
    let right = Expression(in_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForInStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(box VariableDeclaration {
                declarations: vec![box VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: body.1,
        },
    ))
}

fn IterationStatement8(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let decl = ForDeclaration(bracket.0)?;
    let in_ = is_token!(decl.0, Token::KIn)?;
    let right = Expression(in_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForInStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: body.1,
        },
    ))
}

fn IterationStatement9(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    not!(bracket.0, is_token!(Token::KLet))?;
    let left = LeftHandSideExpression(bracket.0)?;
    let of_ = is_token!(left.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::Pattern(ExpressionToPattern(left.1)),
            right: right.1,
            body: body.1,
            await_: false,
        },
    ))
}

fn IterationStatement10(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let var = is_token!(bracket.0, Token::KVar)?;
    let biding = ForBinding(var.0)?;
    let of_ = is_token!(biding.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(box VariableDeclaration {
                declarations: vec![box VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: body.1,
            await_: false,
        },
    ))
}

fn IterationStatement11(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let decl = ForDeclaration(bracket.0)?;
    let of_ = is_token!(decl.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: body.1,
            await_: false,
        },
    ))
}

fn IterationStatement12(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let await_ = is_token!(for_.0, Token::KAwait)?;
    let bracket = is_token!(await_.0, Token::LRound)?;
    not!(bracket.0, is_token!(Token::KLet))?;
    let left = LeftHandSideExpression(bracket.0)?;
    let of_ = is_token!(left.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::Pattern(ExpressionToPattern(left.1)),
            right: right.1,
            body: body.1,
            await_: true,
        },
    ))
}

fn IterationStatement13(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let await_ = is_token!(for_.0, Token::KAwait)?;
    let bracket = is_token!(await_.0, Token::LRound)?;
    let var = is_token!(bracket.0, Token::KVar)?;
    let biding = ForBinding(var.0)?;
    let of_ = is_token!(biding.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(box VariableDeclaration {
                declarations: vec![box VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: body.1,
            await_: true,
        },
    ))
}

fn IterationStatement14(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let await_ = is_token!(for_.0, Token::KAwait)?;
    let bracket = is_token!(await_.0, Token::LRound)?;
    let decl = ForDeclaration(bracket.0)?;
    let of_ = is_token!(decl.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        box node::ForOfStatement {
            left: estree::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: body.1,
            await_: true,
        },
    ))
}

fn ForDeclaration(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclaration>> {
    let let_ = LetOrConst(tokens)?;
    let binding = ForBinding(let_.0)?;
    Ok((
        binding.0,
        box VariableDeclaration {
            declarations: vec![box VariableDeclarator {
                id: binding.1,
                init: None,
            }],
            kind: let_.1.into(),
        },
    ))
}

named!(pub ForBinding<Input<InputWrapper>, Box<estree::Pattern>>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (box res as Box<estree::Pattern>)) |
    BindingPattern
));

fn LexicalDeclaration(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclaration>> {
    let let_ = LetOrConst(tokens)?;
    let binding = BindingList(let_.0)?;
    let semi = is_token!(binding.0, Token::Semicolon)?;

    Ok((
        semi.0,
        box VariableDeclaration {
            declarations: binding.1,
            kind: let_.1.into(),
        },
    ))
}

fn BindingList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::VariableDeclarator>>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), LexicalBinding)
}

named!(pub LexicalBinding<Input<InputWrapper>, Box<estree::VariableDeclarator>>, alt!(
    LexicalBinding1 |
    LexicalBinding2
));

fn LexicalBinding1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclarator>> {
    let ident = BindingIdentifier(tokens)?;
    let init = Initializer(ident.0);

    if let Ok((rest, consumed)) = init {
        return Ok((
            rest,
            box VariableDeclarator {
                id: box ident.1,
                init: Some(consumed),
            },
        ));
    }

    Ok((
        ident.0,
        box VariableDeclarator {
            id: box ident.1,
            init: None,
        },
    ))
}

fn LexicalBinding2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclarator>> {
    let ident = BindingPattern(tokens)?;
    let init = Initializer(ident.0)?;

    Ok((
        init.0,
        box VariableDeclarator {
            id: ident.1,
            init: Some(init.1),
        },
    ))
}

named!(pub LetOrConst<Input<InputWrapper>, Token>, alt!(
    is_token!(Token::KLet) |
    is_token!(Token::KConst)
));

#[inline]
fn either<'a, T>(
    res: &IResult<Input<InputWrapper<'a>>, T>,
    or: Input<InputWrapper<'a>>,
) -> Input<InputWrapper<'a>> {
    if let Ok((res, _)) = res {
        return res.clone();
    }
    or
}

#[inline]
fn unwrap_or_none<T>(res: IResult<Input<InputWrapper>, T>) -> Option<T> {
    if let Ok((_, res)) = res {
        return Some(res);
    }
    None
}

fn IfStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let if_ = is_token!(tokens, Token::KIf)?;
    let bracket = is_token!(if_.0, Token::LRound)?;
    let test = Expression(bracket.0)?;
    let bracket = is_token!(test.0, Token::RRound)?;
    let stmt = Statement(bracket.0)?;
    let else_ = is_token!(stmt.0, Token::KElse);
    if let Ok(else_) = else_ {
        let stmt2 = Statement(else_.0)?;
        return Ok((
            stmt2.0,
            box node::IfStatement {
                test: test.1,
                consequent: stmt.1,
                alternate: Some(stmt2.1),
            },
        ));
    }

    Ok((
        stmt.0,
        box node::IfStatement {
            test: test.1,
            consequent: stmt.1,
            alternate: None,
        },
    ))
}

fn ExpressionStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    not!(tokens, is_token!(Token::LBrace))?;
    not!(tokens, is_token!(Token::KFunction))?;
    let async_ = not!(tokens, is_token!(Token::KAsync))?;
    not!(async_.0, is_token!(Token::KFunction))?;
    not!(tokens, is_token!(Token::KClass))?;
    not!(tokens, is_token!(Token::KLet))?;

    let expression = Expression(tokens)?;
    let semi = is_token!(expression.0, Token::Semicolon)?;
    Ok((
        semi.0,
        box node::ExpressionStatement {
            expression: expression.1,
        },
    ))
}

fn Expression(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let list = separated_nonempty_list!(tokens, is_token!(Token::Comma), AssignmentExpression)?;
    if list.1.len() == 1 {
        return Ok((list.0, list.1[0].box_clone()));
    }
    Ok((
        list.0,
        box node::SequenceExpression {
            expressions: list.1,
        },
    ))
}

fn EmptyStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let empty = is_token!(tokens, Token::Semicolon)?;
    Ok((empty.0, box node::EmptyStatement {}))
}

named!(pub BlockStatement<Input<InputWrapper>, Box<estree::Statement>>, alt!(
    Block
));

fn Block(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    if let Ok((rest, statement)) = StatementList(brackets.0) {
        let brackets = is_token!(rest, Token::RBrace)?;
        return Ok((brackets.0, box BlockStatement { body: statement }));
    }
    let brackets = is_token!(brackets.0, Token::RBrace)?;
    Ok((brackets.0, box BlockStatement { body: vec![] }))
}

fn VariableStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Statement>> {
    let var = take!(tokens, 1)?;
    if (*var.1.inner)[0] != Token::KVar {
        return Err(Err::Error(error_position!(var.0, ErrorKind::Custom(1))));
    }
    let declarations =
        separated_nonempty_list!(var.0, is_token!(Token::Comma), VariableDeclaration)?;
    let semicolon = is_token!(declarations.0, Token::Semicolon)?;
    Ok((
        semicolon.0,
        box VariableDeclaration {
            declarations: declarations.1,
            kind: estree::VariableDeclarationKind::Var,
        },
    ))
}

named!(pub VariableDeclaration<Input<InputWrapper>, Box<estree::VariableDeclarator>>, alt!(
    VariableDeclaration0 | VariableDeclaration1
));

fn VariableDeclaration0(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclarator>> {
    let binding = BindingIdentifier(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            VariableDeclarator::new(
                Box::new(Identifier::from(binding.1)) as Box<estree::Pattern>,
                Some(init.1),
            ),
        ));
    }
    Ok((
        binding.0,
        VariableDeclarator::new(
            Box::new(Identifier::from(binding.1)) as Box<estree::Pattern>,
            None,
        ),
    ))
}

fn VariableDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::VariableDeclarator>> {
    let pattern = BindingPattern(tokens)?;
    let init = Initializer(pattern.0)?;

    Ok((init.0, VariableDeclarator::new(pattern.1, Some(init.1))))
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
    ConditionalExpression |
    /* YieldExpression |*/
    ArrowFunction |
    /* AsyncArrowFunction |*/
    AssignmentExpression1 |
    AssignmentExpression2
));

fn AssignmentExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let left = LeftHandSideExpression(tokens)?;
    let eq = is_token!(left.0, Token::Equal)?;
    let right = AssignmentExpression(eq.0)?;

    Ok((
        right.0,
        box node::AssignmentExpression {
            operator: estree::AssignmentOperator::Equal,
            left: ExpressionToPattern(left.1),
            right: right.1,
        },
    ))
}

fn AssignmentExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let left = LeftHandSideExpression(tokens)?;
    let op = AssignmentOperator(left.0)?;
    let right = AssignmentExpression(op.0)?;

    Ok((
        right.0,
        box node::AssignmentExpression {
            operator: op.1,
            left: ExpressionToPattern(left.1),
            right: right.1,
        },
    ))
}

fn AssignmentOperator(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, estree::AssignmentOperator> {
    let token = take!(tokens, 1)?;
    let res = match (*token.1.inner)[0] {
        Token::MultAssign => estree::AssignmentOperator::TimesEqual,
        Token::SlashEqual => estree::AssignmentOperator::SlashEqual,
        Token::ModAssign => estree::AssignmentOperator::PercentEqual,
        Token::PlusAssign => estree::AssignmentOperator::PlusEqual,
        Token::MinusAssign => estree::AssignmentOperator::MinusEqual,
        Token::DoubleLArrowAssign => estree::AssignmentOperator::LessLessEqual,
        Token::DoubleRArrowAssign => estree::AssignmentOperator::MoreMoreEqual,
        Token::TripleRArrowAssign => estree::AssignmentOperator::MoreMoreMoreEqual,
        Token::AmpAssign => estree::AssignmentOperator::AndEqual,
        Token::CaretAssign => estree::AssignmentOperator::CaretEqual,
        Token::PipeAssign => estree::AssignmentOperator::PipeEqual,
        Token::DoubleStarAssign => estree::AssignmentOperator::StarStarEqual,
        _ => return Err(Err::Error(error_position!(token.0, ErrorKind::Custom(1)))),
    };
    Ok((token.0, res))
}

fn ArrowFunction(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Box<estree::Expression>> {
    let params = ArrowParameters(tokens)?;
    let arrow = is_token!(params.0, Token::EqualArrow)?;
    let body = ConciseBody(arrow.0)?;

    Ok((
        body.0,
        box ArrowFunctionExpression {
            id: None,
            params: params.1,
            async_: false,
            generator: false,
            expression: true,
            body: body.1,
        },
    ))
}

named!(pub ArrowParameters<Input<InputWrapper>, Vec<Box<estree::Pattern>>>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (vec![box res as Box<estree::Pattern>])) |
    CoverParenthesizedExpressionAndArrowParameterList
));

named!(pub ConciseBody<Input<InputWrapper>, estree::ArrowFunctionExpressionBody>, alt!(
    ConciseBody1 |
    ConciseBody2
));

fn ConciseBody1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, estree::ArrowFunctionExpressionBody> {
    not!(tokens, is_token!(Token::LBrace))?;
    let assignment = AssignmentExpression(tokens)?;

    Ok((
        assignment.0,
        estree::ArrowFunctionExpressionBody::Expression(assignment.1),
    ))
}

fn ConciseBody2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, estree::ArrowFunctionExpressionBody> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        estree::ArrowFunctionExpressionBody::FunctionBody(box node::FunctionBody {
            body: body
                .1
                .into_iter()
                .map(|i| estree::FunctionBodyEnum::Statement(i))
                .collect(),
        }),
    ))
}

fn FunctionBody(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Statement>>> {
    FunctionStatementList(tokens)
}

fn FunctionStatementList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Statement>>> {
    let list = StatementList(tokens);
    if let Ok(_) = list {
        return list;
    }
    Ok((tokens, vec![]))
}

named!(pub CoverParenthesizedExpressionAndArrowParameterList<Input<InputWrapper>, Vec<Box<estree::Pattern>>>, alt!(
    CoverParenthesizedExpressionAndArrowParameterList1 |
    CoverParenthesizedExpressionAndArrowParameterList2 |
    CoverParenthesizedExpressionAndArrowParameterList3 |
    CoverParenthesizedExpressionAndArrowParameterList4 |
    CoverParenthesizedExpressionAndArrowParameterList5 |
    CoverParenthesizedExpressionAndArrowParameterList6 |
    CoverParenthesizedExpressionAndArrowParameterList7
));

fn CoverParenthesizedExpressionAndArrowParameterList1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;

    Ok((bracket.0, vec![ExpressionToPattern(expr.1)]))
}

fn CoverParenthesizedExpressionAndArrowParameterList2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(expr.0, Token::Comma)?;
    let bracket = is_token!(comma.0, Token::RRound)?;

    Ok((bracket.0, vec![ExpressionToPattern(expr.1)]))
}

fn CoverParenthesizedExpressionAndArrowParameterList3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let bracket = is_token!(bracket.0, Token::RRound)?;

    Ok((bracket.0, vec![]))
}

fn CoverParenthesizedExpressionAndArrowParameterList4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let dots = is_token!(bracket.0, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![box binding.1]))
}

fn CoverParenthesizedExpressionAndArrowParameterList5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let dots = is_token!(bracket.0, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![binding.1]))
}

fn CoverParenthesizedExpressionAndArrowParameterList6(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(tokens, Token::Comma)?;
    let dots = is_token!(comma.0, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![box binding.1]))
}

fn CoverParenthesizedExpressionAndArrowParameterList7(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Box<estree::Pattern>>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(tokens, Token::Comma)?;
    let dots = is_token!(comma.0, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![binding.1]))
}

fn ExpressionToPattern(expr: Box<estree::Expression>) -> Box<estree::Pattern> {
    if let Ok(ident) = expr.downcast::<Identifier>() {
        return ident as Box<estree::Pattern>;
    }
    // TODO: works only with idents, patterns assignments should be also allowed
    unreachable!()
}

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
    use crate::{
        lexer::Lexer,
        parser::{
            estree::{AssignmentPattern, Identifier, ObjectPattern, VariableDeclaration, *},
            Parser,
        },
    };

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

    #[test]
    fn expression_statement_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref expr) = res.get_body()[0] {
            let expr = expr.downcast_ref::<node::ExpressionStatement>().unwrap();
            let ident = expr.expression.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(ident.get_name(), "a");
        } else {
            unreachable!();
        }
    }

    #[test]
    fn expression_statement_complex() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"a, b;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref expr) = res.get_body()[0] {
            let expr = expr.downcast_ref::<node::ExpressionStatement>().unwrap();
            let expr = expr
                .get_expression()
                .downcast_ref::<node::SequenceExpression>()
                .unwrap();
            let ident = expr.expressions[0]
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(ident.get_name(), "a");
            let ident = expr.expressions[1]
                .downcast_ref::<node::Identifier>()
                .unwrap();
            assert_eq!(ident.get_name(), "b");
        } else {
            unreachable!();
        }
    }

    #[test]
    fn if_statement_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"if (a) {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::IfStatement>().unwrap();
            assert_eq!(
                stmt.test
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            stmt.consequent
                .downcast_ref::<node::BlockStatement>()
                .unwrap();
            assert!(stmt.alternate.is_none());
        } else {
            unreachable!();
        }
    }

    fn if_statement_with_else() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"if (a) {} else b"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::IfStatement>().unwrap();
            assert_eq!(
                stmt.test
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            stmt.consequent
                .downcast_ref::<node::BlockStatement>()
                .unwrap();
            let expr = stmt
                .alternate
                .as_ref()
                .unwrap()
                .downcast_ref::<node::ExpressionStatement>()
                .unwrap();
            expr.get_expression()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn do_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"do ; while (a);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::DoWhileStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            stmt.test.downcast_ref::<node::Identifier>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn while_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"while (a);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::WhileStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            stmt.test.downcast_ref::<node::Identifier>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn for_statement_with_expresions() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(a;a;a);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_some());
            assert!(stmt.test.is_some());
            assert!(stmt.update.is_some());
            if let estree::ForStatementInit::Expression(expr) = stmt.init.as_ref().unwrap() {
                expr.downcast_ref::<node::Identifier>().unwrap();
            } else {
                unreachable!()
            }
            stmt.test
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
            stmt.update
                .as_ref()
                .unwrap()
                .downcast_ref::<node::Identifier>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn for_statement_empty() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(;;);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_none());
            assert!(stmt.test.is_none());
            assert!(stmt.update.is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn for_statement_var() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(var a = 1;;);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_some());
            assert!(stmt.test.is_none());
            assert!(stmt.update.is_none());
            if let estree::ForStatementInit::VariableDeclaration(decl) = stmt.init.as_ref().unwrap()
            {
                assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                let ref declarator = decl.get_declarations()[0];
                assert_eq!(
                    declarator
                        .get_id()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert!(declarator.get_init().is_some());
            } else {
                unreachable!()
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn for_statement_var_multiple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(var a = 1, b = 2;;);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_some());
            assert!(stmt.test.is_none());
            assert!(stmt.update.is_none());
            if let estree::ForStatementInit::VariableDeclaration(decl) = stmt.init.as_ref().unwrap()
            {
                assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                assert_eq!(decl.get_declarations().len(), 2);
                let ref declarator = decl.get_declarations()[0];
                assert_eq!(
                    declarator
                        .get_id()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert!(declarator.get_init().is_some());
            } else {
                unreachable!()
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn for_statement_let() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(let a = 1;;);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_some());
            assert!(stmt.test.is_none());
            assert!(stmt.update.is_none());
            if let estree::ForStatementInit::VariableDeclaration(decl) = stmt.init.as_ref().unwrap()
            {
                assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
                let ref declarator = decl.get_declarations()[0];
                assert_eq!(
                    declarator
                        .get_id()
                        .downcast_ref::<node::Identifier>()
                        .unwrap()
                        .get_name(),
                    "a"
                );
                assert!(declarator.get_init().is_some());
            } else {
                unreachable!()
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    #[ignore]
    fn for_statement_let_pattern() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(let {a};;);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            assert!(stmt.init.is_some());
            assert!(stmt.test.is_none());
            assert!(stmt.update.is_none());
            if let estree::ForStatementInit::VariableDeclaration(decl) = stmt.init.as_ref().unwrap()
            {
                assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
                let ref declarator = decl.get_declarations()[0];
                declarator
                    .get_id()
                    .downcast_ref::<node::ObjectPattern>()
                    .unwrap();
            } else {
                unreachable!()
            }
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forin_statement_assignment() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(a in b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForInStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::Pattern(ref left) = stmt.left {
                left.downcast_ref::<node::Identifier>().unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forin_statement_var() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(var a in b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForInStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forin_statement_const() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(const a in b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForInStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forof_statement_assignment() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::Pattern(ref left) = stmt.left {
                left.downcast_ref::<node::Identifier>().unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forof_statement_var() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(var a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forof_statement_const() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for(const a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forofawait_statement_assignment() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for await(a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::Pattern(ref left) = stmt.left {
                left.downcast_ref::<node::Identifier>().unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, true);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forofawait_statement_var() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for await(var a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, true);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn forofawait_statement_const() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"for await(const a of b);"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ForOfStatement>().unwrap();
            stmt.body.downcast_ref::<node::EmptyStatement>().unwrap();
            if let estree::ForInStatementLeft::VariableDeclaration(ref left) = stmt.left {
                assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                left.get_declarations()[0]
                    .get_id()
                    .downcast_ref::<node::Identifier>()
                    .unwrap();
            }
            stmt.right.downcast_ref::<node::Identifier>().unwrap();
            assert_eq!(stmt.await_, true);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn continue_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"continue;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ContinueStatement>().unwrap();
            assert!(stmt.label.is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn continue_statement_with_label() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"continue aa;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ContinueStatement>().unwrap();
            assert_eq!(stmt.get_label().as_ref().unwrap().get_name(), "aa");
        } else {
            unreachable!();
        }
    }

    #[test]
    fn break_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"break;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::BreakStatement>().unwrap();
            assert!(stmt.label.is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn break_statement_with_label() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"break aa;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::BreakStatement>().unwrap();
            assert_eq!(stmt.get_label().as_ref().unwrap().get_name(), "aa");
        } else {
            unreachable!();
        }
    }

    fn return_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"return;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ReturnStatement>().unwrap();
            assert!(stmt.get_argument().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn return_statement_with_expression() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"return aa;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::ReturnStatement>().unwrap();
            assert_eq!(
                stmt.get_argument()
                    .as_ref()
                    .unwrap()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "aa"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn with_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"with (a) {};"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::WithStatement>().unwrap();
            stmt.object.downcast_ref::<node::Identifier>().unwrap();
            stmt.body.downcast_ref::<node::BlockStatement>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn labeled_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"a: {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::LabeledStatement>().unwrap();
            assert_eq!(stmt.get_label().get_name(), "a");
            stmt.get_body()
                .downcast_ref::<node::BlockStatement>()
                .unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn try_statement_catch() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"try {} catch (a) {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::TryStatement>().unwrap();
            let param = stmt.get_handler().as_ref().unwrap().get_param();
            assert_eq!(
                param.downcast_ref::<node::Identifier>().unwrap().get_name(),
                "a"
            );
            assert!(stmt.get_finalizer().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn try_statement_finally() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"try {} finally {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::TryStatement>().unwrap();
            assert!(stmt.get_handler().is_none());
            let finalizer = stmt.get_finalizer().as_ref().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn try_statement_catch_finally() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"try {} catch (a) {} finally {}"#)
                .unwrap()
                .1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::TryStatement>().unwrap();
            let finalizer = stmt.get_finalizer().as_ref().unwrap();
            let param = stmt.get_handler().as_ref().unwrap().get_param();
            assert_eq!(
                param.downcast_ref::<node::Identifier>().unwrap().get_name(),
                "a"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn debugger_statement() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"debugger;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref stmt) = res.get_body()[0] {
            let stmt = stmt.downcast_ref::<node::DebuggerStatement>().unwrap();
        } else {
            unreachable!();
        }
    }

    #[test]
    fn function_declaration_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"function a() {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::FunctionDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name().clone(), "a");
            assert_eq!(decl.params.len(), 0);
            assert_eq!(decl.generator, false);
            assert_eq!(decl.async_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn function_declaration_simple_arg() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"function a(a, b) {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::FunctionDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name().clone(), "a");
            assert_eq!(decl.params.len(), 2);
            assert_eq!(decl.generator, false);
            assert_eq!(decl.async_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn function_declaration_rest() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"function a(...b) {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::FunctionDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "a");
            assert_eq!(decl.params.len(), 1);
            assert_eq!(decl.generator, false);
            assert_eq!(decl.async_, false);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn class_declaration_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn class_declaration_inheritance() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b extend a {}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(
                decl.super_class
                    .as_ref()
                    .unwrap()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
        } else {
            unreachable!();
        }
    }

    #[test]
    fn method_declaration_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {a() {}}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
            let body = decl.get_body().get_body();
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0]
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
            assert_eq!(body[0].get_static(), false);
            let value = body[0].get_value();
            assert_eq!(value.get_params().len(), 0);
            assert!(value.get_id().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn method_declaration_params() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {a(b,c) {}}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
            let body = decl.get_body().get_body();
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0]
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
            assert_eq!(body[0].get_static(), false);
            let value = body[0].get_value();
            assert_eq!(value.get_params().len(), 2);
            assert!(value.get_id().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn method_declaration_set() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {set a(b) {}}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
            let body = decl.get_body().get_body();
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0]
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Set);
            assert_eq!(body[0].get_static(), false);
            let value = body[0].get_value();
            assert_eq!(value.get_params().len(), 1);
            assert!(value.get_id().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn method_declaration_get() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {get a() {}}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
            let body = decl.get_body().get_body();
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0]
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Get);
            assert_eq!(body[0].get_static(), false);
            let value = body[0].get_value();
            assert_eq!(value.get_params().len(), 0);
            assert!(value.get_id().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn method_declaration_static() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"class b {static a() {}}"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::ClassDeclaration>().unwrap();
            assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
            assert_eq!(decl.super_class.is_none(), true);
            let body = decl.get_body().get_body();
            assert_eq!(body.len(), 1);
            assert_eq!(
                body[0]
                    .get_key()
                    .downcast_ref::<node::Identifier>()
                    .unwrap()
                    .get_name(),
                "a"
            );
            assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
            assert_eq!(body[0].get_static(), true);
            let value = body[0].get_value();
            assert_eq!(value.get_params().len(), 0);
            assert!(value.get_id().is_none());
        } else {
            unreachable!();
        }
    }

    #[test]
    fn lexical_declaration_let_simple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"let a;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::VariableDeclaration>().unwrap();
            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
            let decls = decl.get_declarations();
            assert_eq!(decls.len(), 1);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn lexical_declaration_const_init() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"const a = 1;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::VariableDeclaration>().unwrap();
            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Const);
            let decls = decl.get_declarations();
            assert_eq!(decls.len(), 1);
        } else {
            unreachable!();
        }
    }

    #[test]
    fn lexical_declaration_let_multiple() {
        let res = Parser::ast_tree(
            Lexer::lex_tokens(r#"let a = 1, b = 2;"#).unwrap().1,
            estree::ProgramSourceType::Script,
        );
        if let estree::ProgramBody::ProgramStatement(ref decl) = res.get_body()[0] {
            let decl = decl.downcast_ref::<node::VariableDeclaration>().unwrap();
            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
            let decls = decl.get_declarations();
            assert_eq!(decls.len(), 2);
        } else {
            unreachable!();
        }
    }
}
