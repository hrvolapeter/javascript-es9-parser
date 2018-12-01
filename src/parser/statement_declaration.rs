//! Statement declartion implements transformation of tokens to AST.
//! During transformation all tokens must be consumed and create one AST.
//!
//! For transforming tokens to AST we use nom library. Using `alt` macro we
//! define that grammar rule has multiple results. Parsing starts with first
//! rule and returns as soon as first of the rules
//! succeeds.

use crate::{
    lexer::token::Token,
    parser::{
        estree,
        input_wrapper::InputWrapper,
        node::{self, Expression, Pattern, Statement},
    },
};
use nom::{
    types::{Input, *},
    IResult, *,
};
use std::any::Any;

named!(pub ScriptBody<Input<InputWrapper>, Vec<node::ProgramBody>>, do_parse!(
    res: StatementList >>
    (res.into_iter().map(|s| node::ProgramBody::ProgramStatement(s)).collect())
));

named!(pub StatementList<Input<InputWrapper>, Vec<node::Statement>>, alt!(
    many0!(StatementListItem)
));

named!(pub StatementListItem<Input<InputWrapper>, node::Statement>, alt!(
    Statement |
    do_parse!(res: Declaration >> (res.into()))
));

named!(pub Declaration<Input<InputWrapper>, node::Declaration>, alt!(
    HoistableDeclaration |
    ClassDeclaration |
    do_parse!(res: LexicalDeclaration >> (node::Declaration::VariableDeclaration(res)))
));

named!(pub ClassDeclaration<Input<InputWrapper>, node::Declaration>, alt!(
    ClassDeclaration1 |
    ClassDeclaration2
));

fn ClassDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Declaration> {
    let class = is_token!(tokens, Token::KClass)?;
    let ident = BindingIdentifier(class.0)?;
    let herritage = ClassHeritage(ident.0);
    let bracket = is_token!(either(&herritage, ident.0), Token::LBrace)?;
    let body = ClassBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        node::Declaration::ClassDeclaration(node::ClassDeclaration {
            id: Some(ident.1),
            super_class: unwrap_or_none(herritage),
            body: body.1,
        }),
    ))
}

fn ClassDeclaration2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Declaration> {
    let class = is_token!(tokens, Token::KClass)?;
    let herritage = ClassHeritage(class.0);
    let bracket = is_token!(either(&herritage, class.0), Token::LBrace)?;
    let body = ClassBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    unimplemented!("Annonymous classes are not supported")
}

fn ClassBody(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::ClassBody> {
    let elems = many0!(tokens, ClassElement)?;

    Ok((elems.0, node::ClassBody { body: elems.1 }))
}

named!(pub ClassElement<Input<InputWrapper>, node::MethodDefinition>, alt!(
    ClassElement1 |
    ClassElement2
));

fn ClassElement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::MethodDefinition> {
    let def = MethodDefinition(tokens)?;
    let semi = many0!(def.0, is_token!(Token::Semicolon))?;

    Ok((semi.0, def.1))
}

fn ClassElement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::MethodDefinition> {
    let static_ = is_token!(tokens, Token::KStatic)?;
    let mut def = MethodDefinition(static_.0)?;
    def.1.static_ = true;
    let semi = many0!(def.0, is_token!(Token::Semicolon))?;

    Ok((semi.0, def.1))
}

named!(pub MethodDefinition<Input<InputWrapper>, node::MethodDefinition>, alt!(
    MethodDefinition1 |
    /*GeneratorMethod |
    AsyncMethod |
    AsyncGeneratorMethod |*/
    MethodDefinition5 |
    MethodDefinition6
));

fn MethodDefinition1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::MethodDefinition> {
    let name = PropertyName(tokens)?;
    let bracket = is_token!(name.0, Token::LRound)?;
    let params = UniqueFormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        node::MethodDefinition {
            key: name.1,
            value: node::FunctionExpression {
                id: None,
                params: params.1,
                body: node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| node::FunctionBodyEnum::Statement(i))
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
) -> IResult<Input<InputWrapper>, node::MethodDefinition> {
    let get = is_token!(tokens, Token::KGet)?;
    let name = PropertyName(get.0)?;
    let bracket = is_token!(name.0, Token::LRound)?;
    let bracket = is_token!(bracket.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        node::MethodDefinition {
            key: name.1,
            value: node::FunctionExpression {
                id: None,
                params: vec![],
                body: node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| node::FunctionBodyEnum::Statement(i))
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
) -> IResult<Input<InputWrapper>, node::MethodDefinition> {
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
        node::MethodDefinition {
            key: name.1,
            value: node::FunctionExpression {
                id: None,
                params: vec![param.1],
                body: node::FunctionBody {
                    body: body
                        .1
                        .into_iter()
                        .map(|i| node::FunctionBodyEnum::Statement(i))
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
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    FormalParameters(tokens)
}

fn ClassHeritage(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let extend = is_token!(tokens, Token::KExtend)?;
    LeftHandSideExpression(extend.0)
}

named!(pub HoistableDeclaration<Input<InputWrapper>, node::Declaration>, alt!(
    do_parse!(
        res: FunctionDeclaration >>
        (node::Declaration::FunctionDeclaration(res))) /*|
    GeneratorDeclaration |
    AsyncFunctionDeclaration |
    AsyncGeneratorDeclaration*/
));

named!(pub FunctionDeclaration<Input<InputWrapper>, node::FunctionDeclaration>, alt!(
    FunctionDeclaration1 |
    FunctionDeclaration2
));

fn FunctionDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::FunctionDeclaration> {
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
        node::FunctionDeclaration {
            id: Some(ident.1),
            params: params.1,
            body: node::FunctionBody {
                body: body
                    .1
                    .into_iter()
                    .map(|i| node::FunctionBodyEnum::Statement(i))
                    .collect(),
            },
            generator: false,
            async_: false,
        },
    ))
}

fn FunctionDeclaration2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::FunctionDeclaration> {
    let function = is_token!(tokens, Token::KFunction)?;
    let bracket = is_token!(function.0, Token::LRound)?;
    let params = FormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    unimplemented!("Annonymous functions are not supported")
}

named!(pub FormalParameters<Input<InputWrapper>, Vec<node::Pattern>>, alt!(
    FunctionRestParameter |
    FormalParameterList |
    FormalParameters4 |
    FormalParameters5 |
    FormalParameters1
));

fn FormalParameters1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    Ok((tokens, vec![]))
}

fn FunctionRestParameter(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let rest = BindingRestElement(tokens)?;
    Ok((rest.0, vec![rest.1]))
}

fn FormalParameterList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), FormalParameter)
}

fn FormalParameters4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let list = FormalParameterList(tokens)?;
    let comma = is_token!(list.0, Token::Comma)?;

    Ok((comma.0, list.1))
}

fn FormalParameters5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let mut list = FormalParameterList(tokens)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let rest = BindingRestElement(tokens)?;
    list.1.push(rest.1);

    Ok((rest.0, list.1))
}

fn FormalParameter(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Pattern> {
    BindingElement(tokens)
}
named!(pub Statement<Input<InputWrapper>, node::Statement>, alt!(
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

fn DebuggerStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let debugger = is_token!(tokens, Token::KDebugger)?;
    let semicolon = is_token!(debugger.0, Token::Semicolon)?;
    Ok((
        semicolon.0,
        node::Statement::DebuggerStatement(node::DebuggerStatement {}),
    ))
}

named!(pub TryStatement<Input<InputWrapper>, node::Statement>, alt!(
    TryStatement3 |
    TryStatement2 |
    TryStatement1
));

fn TryStatement1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let catch = Catch(block.0)?;
    Ok((
        catch.0,
        node::Statement::TryStatement(node::TryStatement {
            block: block.1,
            handler: Some(catch.1),
            finalizer: None,
        }),
    ))
}

fn TryStatement2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let finally = Finally(block.0)?;
    Ok((
        finally.0,
        node::Statement::TryStatement(node::TryStatement {
            block: block.1,
            handler: None,
            finalizer: Some(finally.1),
        }),
    ))
}

fn TryStatement3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let try_ = is_token!(tokens, Token::KTry)?;
    let block = Block(try_.0)?;
    let catch = Catch(block.0)?;
    let finally = Finally(catch.0)?;
    Ok((
        finally.0,
        node::Statement::TryStatement(node::TryStatement {
            block: block.1,
            handler: Some(catch.1),
            finalizer: Some(finally.1),
        }),
    ))
}

fn Finally(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::BlockStatement> {
    let finally = is_token!(tokens, Token::KFinally)?;
    Block(finally.0)
}

fn Catch(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::CatchClause> {
    let catch = is_token!(tokens, Token::KCatch)?;
    let bracket = is_token!(catch.0, Token::LRound)?;
    let param = CatchParameter(bracket.0)?;
    let bracket = is_token!(param.0, Token::RRound)?;
    let block = Block(bracket.0)?;
    Ok((
        block.0,
        node::CatchClause {
            param: param.1,
            body: block.1,
        },
    ))
}

named!(pub CatchParameter<Input<InputWrapper>, node::Pattern>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (node::Pattern::Identifier(res))) |
    BindingPattern
));

fn ThrowStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let throw = is_token!(tokens, Token::KThrow)?;
    let expr = Expression(throw.0)?;
    let semi = is_token!(expr.0, Token::Semicolon)?;
    Ok((
        semi.0,
        node::Statement::ThrowStatement(node::ThrowStatement { argument: expr.1 }),
    ))
}

fn LabeledStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let ident = LabelIdentifier(tokens)?;
    let colon = is_token!(ident.0, Token::Colon)?;
    let item = LabeledItem(colon.0)?;
    Ok((
        item.0,
        node::Statement::LabeledStatement(node::LabeledStatement {
            label: ident.1,
            body: box item.1,
        }),
    ))
}

named!(pub LabeledItem<Input<InputWrapper>, node::Statement>, alt!(
    Statement |
    do_parse!(
        res: FunctionDeclaration >>
        (node::Statement::FunctionDeclaration(res)))
));

fn WithStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let with = is_token!(tokens, Token::KWith)?;
    let bracket = is_token!(with.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let stmt = Statement(bracket.0)?;
    Ok((
        stmt.0,
        node::Statement::WithStatement(node::WithStatement {
            object: expr.1,
            body: box stmt.1,
        }),
    ))
}

named!(pub ReturnStatement<Input<InputWrapper>, node::Statement>, alt!(
    ReturnStatement1 |
    ReturnStatement2
));

fn ReturnStatement1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KReturn)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((
        semi.0,
        node::Statement::ReturnStatement(node::ReturnStatement { argument: None }),
    ))
}

fn ReturnStatement2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KReturn)?;
    let expr = Expression(con.0)?;
    let semi = is_token!(expr.0, Token::Semicolon)?;

    Ok((
        semi.0,
        node::Statement::ReturnStatement(node::ReturnStatement {
            argument: Some(expr.1),
        }),
    ))
}

named!(pub BreakStatement<Input<InputWrapper>, node::Statement>, alt!(
    BreakStatement1 |
    BreakStatement2
));

fn BreakStatement1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KBreak)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((
        semi.0,
        node::Statement::BreakStatement(node::BreakStatement { label: None }),
    ))
}

fn BreakStatement2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KBreak)?;
    let label = LabelIdentifier(con.0)?;
    let semi = is_token!(label.0, Token::Semicolon)?;

    Ok((
        semi.0,
        node::Statement::BreakStatement(node::BreakStatement {
            label: Some(label.1),
        }),
    ))
}

named!(pub ContinueStatement<Input<InputWrapper>, node::Statement>, alt!(
    ContinueStatement1 |
    ContinueStatement2
));

fn ContinueStatement1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KContinue)?;
    let semi = is_token!(con.0, Token::Semicolon)?;
    Ok((
        semi.0,
        node::Statement::ContinueStatement(node::ContinueStatement { label: None }),
    ))
}

fn ContinueStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    let con = is_token!(tokens, Token::KContinue)?;
    let label = LabelIdentifier(con.0)?;
    let semi = is_token!(label.0, Token::Semicolon)?;

    Ok((
        semi.0,
        node::Statement::ContinueStatement(node::ContinueStatement {
            label: Some(label.1),
        }),
    ))
}

fn LabelIdentifier(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Identifier> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, ident.clone().into()));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub BrekableStatement<Input<InputWrapper>, node::Statement>, alt!(
    IterationStatement |
    SwitchStatement
));

fn SwitchStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let switch_ = is_token!(tokens, Token::KSwitch)?;
    let bracket = is_token!(switch_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let case = CaseBlock(bracket.0)?;
    Ok((
        case.0,
        node::Statement::SwitchStatement(node::SwitchStatement {
            discriminant: expr.1,
            case: case.1,
        }),
    ))
}

named!(pub CaseBlock<Input<InputWrapper>, Vec<node::SwitchCase>>, alt!(
    CaseBlock1 |
    CaseBlock2
));

fn CaseBlock1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Vec<node::SwitchCase>> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let clauses = many0!(bracket.0, CaseClause);
    if let Ok((rest, consumed)) = clauses {
        return Ok((rest, consumed));
    }
    let bracket = is_token!(bracket.0, Token::RBrace)?;
    Ok((bracket.0, vec![]))
}

fn CaseBlock2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Vec<node::SwitchCase>> {
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

fn DefaultClause(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::SwitchCase> {
    let default = is_token!(tokens, Token::KDefault)?;
    let colon = is_token!(default.0, Token::Colon)?;
    let statements = StatementList(colon.0);

    if let Ok((rest, consumed)) = statements {
        return Ok((
            rest,
            node::SwitchCase {
                test: None,
                consequent: consumed,
            },
        ));
    }

    Ok((
        colon.0,
        node::SwitchCase {
            test: None,
            consequent: vec![],
        },
    ))
}

fn CaseClause(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::SwitchCase> {
    let case = is_token!(tokens, Token::KCase)?;
    let expr = Expression(case.0)?;
    let colon = is_token!(expr.0, Token::Colon)?;
    let statements = StatementList(colon.0);

    if let Ok((rest, consumed)) = statements {
        return Ok((
            rest,
            node::SwitchCase {
                test: Some(expr.1),
                consequent: consumed,
            },
        ));
    }

    Ok((
        colon.0,
        node::SwitchCase {
            test: Some(expr.1),
            consequent: vec![],
        },
    ))
}

named!(pub IterationStatement<Input<InputWrapper>, node::Statement>, alt!(
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
) -> IResult<Input<InputWrapper>, node::Statement> {
    let do_ = is_token!(tokens, Token::KDo)?;
    let stmt = Statement(do_.0)?;
    let while_ = is_token!(stmt.0, Token::KWhile)?;
    let bracket = is_token!(while_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let semi = is_token!(bracket.0, Token::Semicolon)?;
    Ok((
        semi.0,
        node::Statement::DoWhileStatement(node::DoWhileStatement {
            body: box stmt.1,
            test: box expr.1,
        }),
    ))
}

fn IterationStatement2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    let while_ = is_token!(tokens, Token::KWhile)?;
    let bracket = is_token!(while_.0, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;
    let stmt = Statement(bracket.0)?;

    Ok((
        stmt.0,
        node::Statement::WhileStatement(node::WhileStatement {
            body: box stmt.1,
            test: expr.1,
        }),
    ))
}

fn IterationStatement3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForStatement(node::ForStatement {
            init: unwrap_or_none(expr1).map(|i| node::ForStatementInit::Expression(i)),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: box stmt.1,
        }),
    ))
}

fn IterationStatement4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForStatement(node::ForStatement {
            init: Some(node::ForStatementInit::VariableDeclaration(
                node::VariableDeclaration {
                    declarations: declarations.1,
                    kind: estree::VariableDeclarationKind::Var,
                },
            )),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: box stmt.1,
        }),
    ))
}

fn IterationStatement5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForStatement(node::ForStatement {
            init: Some(node::ForStatementInit::VariableDeclaration(lex.1)),
            test: unwrap_or_none(expr2),
            update: unwrap_or_none(expr3),
            body: box stmt.1,
        }),
    ))
}

fn IterationStatement6(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForInStatement(node::ForInStatement {
            left: node::ForInStatementLeft::Pattern(left.1.into()),
            right: right.1,
            body: box body.1,
        }),
    ))
}

fn IterationStatement7(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForInStatement(node::ForInStatement {
            left: node::ForInStatementLeft::VariableDeclaration(node::VariableDeclaration {
                declarations: vec![node::VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: box body.1,
        }),
    ))
}

fn IterationStatement8(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let decl = ForDeclaration(bracket.0)?;
    let in_ = is_token!(decl.0, Token::KIn)?;
    let right = Expression(in_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        node::Statement::ForInStatement(node::ForInStatement {
            left: node::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: box body.1,
        }),
    ))
}

fn IterationStatement9(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::Pattern(left.1.into()),
            right: right.1,
            body: box body.1,
            await_: false,
        }),
    ))
}

fn IterationStatement10(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::VariableDeclaration(node::VariableDeclaration {
                declarations: vec![node::VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: box body.1,
            await_: false,
        }),
    ))
}

fn IterationStatement11(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    let for_ = is_token!(tokens, Token::KFor)?;
    let bracket = is_token!(for_.0, Token::LRound)?;
    let decl = ForDeclaration(bracket.0)?;
    let of_ = is_token!(decl.0, Token::KOf)?;
    let right = AssignmentExpression(of_.0)?;
    let bracket = is_token!(right.0, Token::RRound)?;
    let body = Statement(bracket.0)?;

    Ok((
        body.0,
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: box body.1,
            await_: false,
        }),
    ))
}

fn IterationStatement12(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::Pattern(left.1.into()),
            right: right.1,
            body: box body.1,
            await_: true,
        }),
    ))
}

fn IterationStatement13(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::VariableDeclaration(node::VariableDeclaration {
                declarations: vec![node::VariableDeclarator {
                    init: None,
                    id: biding.1,
                }],
                kind: estree::VariableDeclarationKind::Var,
            }),
            right: right.1,
            body: box body.1,
            await_: true,
        }),
    ))
}

fn IterationStatement14(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ForOfStatement(node::ForOfStatement {
            left: node::ForInStatementLeft::VariableDeclaration(decl.1),
            right: right.1,
            body: box body.1,
            await_: true,
        }),
    ))
}

fn ForDeclaration(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclaration> {
    let let_ = LetOrConst(tokens)?;
    let binding = ForBinding(let_.0)?;
    Ok((
        binding.0,
        node::VariableDeclaration {
            declarations: vec![node::VariableDeclarator {
                id: binding.1,
                init: None,
            }],
            kind: let_.1.into(),
        },
    ))
}

named!(pub ForBinding<Input<InputWrapper>, node::Pattern>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (node::Pattern::Identifier(res))) |
    BindingPattern
));

fn LexicalDeclaration(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclaration> {
    let let_ = LetOrConst(tokens)?;
    let binding = BindingList(let_.0)?;
    let semi = is_token!(binding.0, Token::Semicolon)?;

    Ok((
        semi.0,
        node::VariableDeclaration {
            declarations: binding.1,
            kind: let_.1.into(),
        },
    ))
}

fn BindingList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::VariableDeclarator>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), LexicalBinding)
}

named!(pub LexicalBinding<Input<InputWrapper>, node::VariableDeclarator>, alt!(
    LexicalBinding1 |
    LexicalBinding2
));

fn LexicalBinding1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclarator> {
    let ident = BindingIdentifier(tokens)?;
    let init = Initializer(ident.0);

    if let Ok((rest, consumed)) = init {
        return Ok((
            rest,
            node::VariableDeclarator {
                id: node::Pattern::Identifier(ident.1),
                init: Some(consumed),
            },
        ));
    }

    Ok((
        ident.0,
        node::VariableDeclarator {
            id: node::Pattern::Identifier(ident.1),
            init: None,
        },
    ))
}

fn LexicalBinding2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclarator> {
    let ident = BindingPattern(tokens)?;
    let init = Initializer(ident.0)?;

    Ok((
        init.0,
        node::VariableDeclarator {
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

fn IfStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
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
            node::Statement::IfStatement(node::IfStatement {
                test: test.1,
                consequent: box stmt.1,
                alternate: Some(box stmt2.1),
            }),
        ));
    }

    Ok((
        stmt.0,
        node::Statement::IfStatement(node::IfStatement {
            test: test.1,
            consequent: box stmt.1,
            alternate: None,
        }),
    ))
}

fn ExpressionStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
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
        node::Statement::ExpressionStatement(node::ExpressionStatement {
            expression: box expression.1,
        }),
    ))
}

fn Expression(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let list = separated_nonempty_list!(tokens, is_token!(Token::Comma), AssignmentExpression)?;
    if list.1.len() == 1 {
        return Ok((list.0, list.1[0].clone()));
    }
    Ok((
        list.0,
        node::Expression::SequenceExpression(node::SequenceExpression {
            expressions: list.1,
        }),
    ))
}

fn EmptyStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let empty = is_token!(tokens, Token::Semicolon)?;
    Ok((
        empty.0,
        node::Statement::EmptyStatement(node::EmptyStatement {}),
    ))
}

named!(pub BlockStatement<Input<InputWrapper>, node::Statement>, alt!(
    do_parse!(res: Block >> (node::Statement::BlockStatement(res)))
));

fn Block(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::BlockStatement> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    if let Ok((rest, statement)) = StatementList(brackets.0) {
        let brackets = is_token!(rest, Token::RBrace)?;
        return Ok((brackets.0, node::BlockStatement { body: statement }));
    }
    let brackets = is_token!(brackets.0, Token::RBrace)?;
    Ok((brackets.0, node::BlockStatement { body: vec![] }))
}

fn VariableStatement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Statement> {
    let var = take!(tokens, 1)?;
    if (*var.1.inner)[0] != Token::KVar {
        return Err(Err::Error(error_position!(var.0, ErrorKind::Custom(1))));
    }
    let declarations =
        separated_nonempty_list!(var.0, is_token!(Token::Comma), VariableDeclaration)?;
    let semicolon = is_token!(declarations.0, Token::Semicolon)?;
    Ok((
        semicolon.0,
        node::Statement::VariableDeclaration(node::VariableDeclaration {
            declarations: declarations.1,
            kind: estree::VariableDeclarationKind::Var,
        }),
    ))
}

named!(pub VariableDeclaration<Input<InputWrapper>, node::VariableDeclarator>, alt!(
    VariableDeclaration0 | VariableDeclaration1
));

fn VariableDeclaration0(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclarator> {
    let binding = BindingIdentifier(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            node::VariableDeclarator {
                id: node::Pattern::Identifier(node::Identifier::from(binding.1)),
                init: Some(init.1),
            },
        ));
    }
    Ok((
        binding.0,
        node::VariableDeclarator {
            id: node::Pattern::Identifier(node::Identifier::from(binding.1)),
            init: None,
        },
    ))
}

fn VariableDeclaration1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::VariableDeclarator> {
    let pattern = BindingPattern(tokens)?;
    let init = Initializer(pattern.0)?;

    Ok((
        init.0,
        node::VariableDeclarator {
            id: pattern.1,
            init: Some(init.1),
        },
    ))
}

fn BindingIdentifier(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Identifier> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        return Ok((identifier.0, ident.clone().into()));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub BindingPattern<Input<InputWrapper>, node::Pattern>, alt!(
    ObjectBindingPattern | ArrayBindingPattern
));

named!(pub ObjectBindingPattern<Input<InputWrapper>, node::Pattern>, do_parse!(
    res: alt!(
        ObjectBindingPattern1 |
        ObjectBindingPattern2 |
        ObjectBindingPattern3 |
        ObjectBindingPattern4
    ) >>
    (node::Pattern::ObjectPattern(res))
));

fn ObjectBindingPattern1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let brackets = is_token!(brackets.0, Token::RBrace)?;
    Ok((brackets.0, node::ObjectPattern { properties: vec![] }))
}

fn ObjectBindingPattern2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let rest = BindingRestProperty(brackets.0)?;
    let brackets = is_token!(rest.0, Token::RBrace)?;

    Ok((
        brackets.0,
        node::ObjectPattern {
            properties: vec![node::ObjectPatternProperty::RestElement(rest.1)],
        },
    ))
}

fn BindingRestProperty(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::RestElement> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let ident = BindingIdentifier(dots.0)?;
    Ok((
        ident.0,
        node::RestElement {
            argument: box node::Pattern::Identifier(ident.1),
        },
    ))
}

fn BindingPropertyList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::ObjectPatternProperty>> {
    separated_nonempty_list!(tokens, is_token!(Token::Comma), BindingProperty)
}

use nom::dbg;
named!(pub BindingProperty<Input<InputWrapper>, node::ObjectPatternProperty>, alt!(
    BindingProperty2 |
    BindingProperty1
));

fn SingleNameBinding(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Pattern> {
    let binding = BindingIdentifier(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            node::Pattern::AssignmentPattern(node::AssignmentPattern {
                left: box node::Pattern::Identifier(binding.1),
                right: box init.1,
            }),
        ));
    }
    Ok((binding.0, node::Pattern::Identifier(binding.1)))
}

fn BindingProperty1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPatternProperty> {
    let simple = SingleNameBinding(tokens)?;
    let key = match simple.1 {
        node::Pattern::AssignmentPattern(ref assign) => {
            if let node::Pattern::Identifier(ident) = &*assign.left {
                node::Expression::Identifier(ident.clone())
            } else {
                unreachable!()
            }
        }
        node::Pattern::Identifier(ref ident) => node::Expression::Identifier(ident.clone()),
        _ => unreachable!(),
    };
    Ok((
        simple.0,
        node::ObjectPatternProperty::AssignmentProperty(node::AssignmentProperty {
            key,
            value: simple.1,
            shorthand: true,
        }),
    ))
}

fn BindingProperty2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPatternProperty> {
    let binding = PropertyName(tokens)?;
    let colon = is_token!(binding.0, Token::Colon)?;
    let init = BindingElement(colon.0)?;
    Ok((
        init.0,
        node::ObjectPatternProperty::AssignmentProperty(node::AssignmentProperty {
            key: binding.1,
            value: init.1,
            shorthand: false,
        }),
    ))
}

named!(pub LiteralPropertyName<Input<InputWrapper>, node::Expression>, alt!(
    IdentifierName |
    StringLiteral |
    NumericLiteral
));

fn IdentifierName(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &(*identifier.1.inner)[0] {
        let ident: node::Identifier = ident.clone().into();
        return Ok((identifier.0, node::Expression::Identifier(ident)));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn StringLiteral(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::StringLiteral(ident) = &(*identifier.1.inner)[0] {
        return Ok((
            identifier.0,
            node::Expression::StringLiteral(node::StringLiteral(ident.clone())),
        ));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn NumericLiteral(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::NumericLiteral(ident) = &(*identifier.1.inner)[0] {
        let ident: node::NumberLiteral = node::NumberLiteral(ident.clone());
        return Ok((identifier.0, node::Expression::NumberLiteral(ident)));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn NullLiteral(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::LNull = &(*identifier.1.inner)[0] {
        return Ok((
            identifier.0,
            node::Expression::NullLiteral(node::NullLiteral),
        ));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

fn BooleanLiteral(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::BoolLiteral(b) = (*identifier.1.inner)[0] {
        return Ok((
            identifier.0,
            node::Expression::BooleanLiteral(node::BooleanLiteral(b)),
        ));
    }
    Err(Err::Error(error_position!(
        identifier.0,
        ErrorKind::Custom(1)
    )))
}

named!(pub PropertyName<Input<InputWrapper>, node::Expression>, alt!(
    LiteralPropertyName |
    ComputedPropertyName
));

fn ComputedPropertyName(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let assign = AssignmentExpression(bracket.0)?;
    let bracket = is_token!(assign.0, Token::RSquare)?;
    Ok((bracket.0, assign.1))
}

named!(pub BindingElement<Input<InputWrapper>, node::Pattern>, alt!(
    SingleNameBinding |
    BindingElement1
));

fn BindingElement1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Pattern> {
    let binding = BindingPattern(tokens)?;
    let init = Initializer(binding.0);
    if let Ok(init) = init {
        return Ok((
            init.0,
            node::Pattern::AssignmentPattern(node::AssignmentPattern {
                left: box binding.1,
                right: box init.1,
            }),
        ));
    }

    Ok(binding)
}

fn ObjectBindingPattern3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let list = BindingPropertyList(brackets.0)?;
    let brackets = is_token!(list.0, Token::RBrace)?;
    Ok((brackets.0, node::ObjectPattern { properties: list.1 }))
}

fn ObjectBindingPattern4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectPattern> {
    let brackets = is_token!(tokens, Token::LBrace)?;
    let list = BindingPropertyList(brackets.0)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let rest = BindingRestProperty(list.0);
    let brackets = if let Ok(rest) = rest {
        is_token!(rest.0, Token::RBrace)?
    } else {
        is_token!(comma.0, Token::RBrace)?
    };
    Ok((brackets.0, node::ObjectPattern { properties: list.1 }))
}

named!(pub ArrayBindingPattern<Input<InputWrapper>, node::Pattern>, do_parse!(
    res: alt!(
        ArrayBindingPattern1 |
        ArrayBindingPattern2 |
        ArrayBindingPattern3
    ) >> (node::Pattern::ArrayPattern(res))
));

fn ArrayBindingPattern1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrayPattern> {
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
    Ok((bracket.0, node::ArrayPattern { elements: elision }))
}

named!(pub BindingRestElement<Input<InputWrapper>, node::Pattern>, alt!(
    BindingRestElement1 |
    BindingRestElement2
));

fn BindingRestElement1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Pattern> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    Ok((
        binding.0,
        node::Pattern::RestElement(node::RestElement {
            argument: box node::Pattern::Identifier(binding.1),
        }),
    ))
}

fn BindingRestElement2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Pattern> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    Ok((
        binding.0,
        node::Pattern::RestElement(node::RestElement {
            argument: box binding.1,
        }),
    ))
}

fn Elision(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<node::Pattern>>> {
    let res = many0!(tokens, is_token!(Token::Comma))?;
    Ok((res.0, res.1.into_iter().map(|_| None).collect()))
}

fn ArrayBindingPattern2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrayPattern> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let binding = BindingElementList(bracket.0)?;
    let bracket = is_token!(binding.0, Token::RSquare)?;
    Ok((
        bracket.0,
        node::ArrayPattern {
            elements: binding.1,
        },
    ))
}

fn BindingElementList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<node::Pattern>>> {
    let list = separated_nonempty_list!(tokens, is_token!(Token::Comma), BindingElisionElement)?;
    Ok((list.0, list.1.into_iter().flatten().collect()))
}

fn BindingElisionElement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<Option<node::Pattern>>> {
    if let Ok((rest, mut elision)) = Elision(tokens) {
        let binding = BindingElement(rest)?;
        elision.push(Some(binding.1));
        return Ok((binding.0, elision));
    }
    let binding = BindingElement(tokens)?;
    Ok((binding.0, vec![Some(binding.1)]))
}

fn ArrayBindingPattern3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrayPattern> {
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
        node::ArrayPattern {
            elements: binding.1,
        },
    ))
}

fn Initializer(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let eq = is_token!(tokens, Token::Assign)?;
    AssignmentExpression(eq.0)
}

named!(pub AssignmentExpression<Input<InputWrapper>, node::Expression>, alt!(
    /* YieldExpression |*/
    ArrowFunction |
    /* AsyncArrowFunction |*/
    AssignmentExpression2 |
    AssignmentExpression1 |
    ConditionalExpression
));

fn AssignmentExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let left = LeftHandSideExpression(tokens)?;
    let eq = is_token!(left.0, Token::Assign)?;
    let right = AssignmentExpression(eq.0)?;

    Ok((
        right.0,
        node::Expression::AssignmentExpression(node::AssignmentExpression {
            operator: estree::AssignmentOperator::Equal,
            left: box left.1.into(),
            right: box right.1,
        }),
    ))
}

fn AssignmentExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let left = LeftHandSideExpression(tokens)?;
    let op = AssignmentOperator(left.0)?;
    let right = AssignmentExpression(op.0)?;

    Ok((
        right.0,
        node::Expression::AssignmentExpression(node::AssignmentExpression {
            operator: op.1,
            left: box left.1.into(),
            right: box right.1,
        }),
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

fn ArrowFunction(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let params = ArrowParameters(tokens)?;
    let arrow = is_token!(params.0, Token::EqualArrow)?;
    let body = ConciseBody(arrow.0)?;

    Ok((
        body.0,
        node::Expression::ArrowFunctionExpression(node::ArrowFunctionExpression {
            id: None,
            params: params.1,
            async_: false,
            generator: false,
            expression: true,
            body: body.1,
        }),
    ))
}

named!(pub ArrowParameters<Input<InputWrapper>, Vec<node::Pattern>>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (vec![node::Pattern::Identifier(res)])) |
    CoverParenthesizedExpressionAndArrowParameterList
));

named!(pub ConciseBody<Input<InputWrapper>, node::ArrowFunctionExpressionBody>, alt!(
    ConciseBody1 |
    ConciseBody2
));

fn ConciseBody1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrowFunctionExpressionBody> {
    not!(tokens, is_token!(Token::LBrace))?;
    let assignment = AssignmentExpression(tokens)?;

    Ok((
        assignment.0,
        node::ArrowFunctionExpressionBody::Expression(box assignment.1),
    ))
}

fn ConciseBody2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrowFunctionExpressionBody> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;

    Ok((
        bracket.0,
        node::ArrowFunctionExpressionBody::FunctionBody(node::FunctionBody {
            body: body
                .1
                .into_iter()
                .map(|i| node::FunctionBodyEnum::Statement(i))
                .collect(),
        }),
    ))
}

fn FunctionBody(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, Vec<node::Statement>> {
    FunctionStatementList(tokens)
}

fn FunctionStatementList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Statement>> {
    let list = StatementList(tokens);
    if let Ok(_) = list {
        return list;
    }
    Ok((tokens, vec![]))
}

named!(pub CoverParenthesizedExpressionAndArrowParameterList<Input<InputWrapper>, Vec<node::Pattern>>, alt!(
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
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let bracket = is_token!(expr.0, Token::RRound)?;

    Ok((bracket.0, vec![expr.1.into()]))
}

fn CoverParenthesizedExpressionAndArrowParameterList2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(expr.0, Token::Comma)?;
    let bracket = is_token!(comma.0, Token::RRound)?;

    Ok((bracket.0, vec![expr.1.into()]))
}

fn CoverParenthesizedExpressionAndArrowParameterList3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let bracket = is_token!(bracket.0, Token::RRound)?;

    Ok((bracket.0, vec![]))
}

fn CoverParenthesizedExpressionAndArrowParameterList4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let dots = is_token!(bracket.0, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![node::Pattern::Identifier(binding.1)]))
}

fn CoverParenthesizedExpressionAndArrowParameterList5(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let dots = is_token!(bracket.0, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![binding.1]))
}

fn CoverParenthesizedExpressionAndArrowParameterList6(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(tokens, Token::Comma)?;
    let dots = is_token!(comma.0, Token::TripleDot)?;
    let binding = BindingIdentifier(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![node::Pattern::Identifier(binding.1)]))
}

fn CoverParenthesizedExpressionAndArrowParameterList7(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Pattern>> {
    let bracket = is_token!(tokens, Token::LRound)?;
    let expr = Expression(bracket.0)?;
    let comma = is_token!(tokens, Token::Comma)?;
    let dots = is_token!(comma.0, Token::TripleDot)?;
    let binding = BindingPattern(dots.0)?;
    let bracket = is_token!(binding.0, Token::RRound)?;

    Ok((bracket.0, vec![binding.1]))
}

named!(pub ConditionalExpression<Input<InputWrapper>, node::Expression>, alt!(
    LogicalORExpression
));

named!(pub LogicalORExpression<Input<InputWrapper>, node::Expression>, alt!(
    LogicalORExpression1 |
    LogicalANDExpression
));

fn LogicalORExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Or)?;
    let or = LogicalORExpression(skipped.1)?;
    let and = LogicalORExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::LogicalExpression(node::LogicalExpression {
            operator: estree::LogicalOperator::Or,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub LogicalANDExpression<Input<InputWrapper>, node::Expression>, alt!(
    LogicalANDExpression1 |
    BitwiseORExpression
));

fn LogicalANDExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::And)?;
    let or = LogicalANDExpression(skipped.1)?;
    let and = LogicalANDExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::LogicalExpression(node::LogicalExpression {
            operator: estree::LogicalOperator::And,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub BitwiseORExpression<Input<InputWrapper>, node::Expression>, alt!(
    BitwiseXORExpression
));

named!(pub BitwiseXORExpression<Input<InputWrapper>, node::Expression>, alt!(
    BitwiseANDExpression
));

named!(pub BitwiseANDExpression<Input<InputWrapper>, node::Expression>, alt!(
    EqualityExpression
));

named!(pub EqualityExpression<Input<InputWrapper>, node::Expression>, alt!(
    EqualityExpression1 |
    EqualityExpression2 |
    EqualityExpression3 |
    EqualityExpression4 |
    RelationalExpression
));

fn EqualityExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Equal)?;
    let or = EqualityExpression(skipped.1)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::EqualEqual,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::NotEqual)?;
    let or = EqualityExpression(skipped.1)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::ExclamationEqual,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::EqualEqual)?;
    let or = EqualityExpression(skipped.1)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::EqualEqualEqual,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::NotEqualEqual)?;
    let or = EqualityExpression(skipped.1)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::ExclamationEqualEqual,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub RelationalExpression<Input<InputWrapper>, node::Expression>, alt!(
    ShiftExpression
));

named!(pub ShiftExpression<Input<InputWrapper>, node::Expression>, alt!(
    ShiftExpression1 |
    ShiftExpression2 |
    ShiftExpression3 |
    AdditiveExpression
));

fn ShiftExpression1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::DoubleLAngle)?;
    let or = ShiftExpression(skipped.1)?;
    let and = ShiftExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::LessLess,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn ShiftExpression2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::DoubleRAngle)?;
    let or = ShiftExpression(skipped.1)?;
    let and = ShiftExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::MoreMore,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn ShiftExpression3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::TripleRAngle)?;
    let or = ShiftExpression(skipped.1)?;
    let and = ShiftExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::MoreMoreMore,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub AdditiveExpression<Input<InputWrapper>, node::Expression>, alt!(
    AdditiveExpression1 |
    AdditiveExpression2 |
    MultiplicativeExpression
));

fn AdditiveExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Plus)?;
    let or = AdditiveExpression(skipped.1)?;
    let and = AdditiveExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Plus,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn AdditiveExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Minus)?;
    let or = AdditiveExpression(skipped.1)?;
    let and = AdditiveExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Minus,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub MultiplicativeExpression<Input<InputWrapper>, node::Expression>, alt!(
    MultiplicativeExpression1 |
    MultiplicativeExpression2 |
    MultiplicativeExpression3 |
    ExponentiationExpression
));

fn MultiplicativeExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Mult)?;
    let or = MultiplicativeExpression(skipped.1)?;
    let and = MultiplicativeExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Times,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn MultiplicativeExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Mod)?;
    let or = MultiplicativeExpression(skipped.1)?;
    let and = MultiplicativeExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Percent,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

fn MultiplicativeExpression3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::Slash)?;
    let or = MultiplicativeExpression(skipped.1)?;
    let and = MultiplicativeExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Slash,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub ExponentiationExpression<Input<InputWrapper>, node::Expression>, alt!(
    ExponentiationExpression1 |
    UnaryExpression
));

fn ExponentiationExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::DoubleMult)?;
    let or = UnaryExpression(skipped.1)?;
    let and = ExponentiationExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::StarStar,
            left: box or.1,
            right: box and.1,
        }),
    ))
}

named!(pub UnaryExpression<Input<InputWrapper>, node::Expression>, alt!(
    UnaryExpression1 |
    UnaryExpression2 |
    UnaryExpression3 |
    UnaryExpression4 |
    UnaryExpression5 |
    UnaryExpression6 |
    UnaryExpression7 |
    UpdateExpression
));

fn UnaryExpression1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::KDelete)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Delete,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::KVoid)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Void,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::KTypeof)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Typeof,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression4(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Plus)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Plus,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression5(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Minus)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Minus,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression6(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Tilde)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Tilde,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

fn UnaryExpression7(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Exclamation)?;
    let un = UnaryExpression(tok.0)?;

    Ok((
        un.0,
        node::Expression::UnaryExpression(node::UnaryExpression {
            operator: estree::UnaryOperator::Exclamation,
            prefix: true,
            argument: box un.1,
        }),
    ))
}

named!(pub UpdateExpression<Input<InputWrapper>, node::Expression>, alt!(
    UpdateExpression1 |
    UpdateExpression2 |
    UpdateExpression3 |
    UpdateExpression4 |
    LeftHandSideExpression
));

fn UpdateExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let ex = LeftHandSideExpression(tokens)?;
    let tok = is_token!(ex.0, Token::DoublePlus)?;

    Ok((
        tok.0,
        node::Expression::UpdateExpression(node::UpdateExpression {
            operator: estree::UpdateOperator::PlusPlus,
            prefix: false,
            argument: box ex.1,
        }),
    ))
}

fn UpdateExpression2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let ex = LeftHandSideExpression(tokens)?;
    let tok = is_token!(ex.0, Token::DoubleMinus)?;

    Ok((
        tok.0,
        node::Expression::UpdateExpression(node::UpdateExpression {
            operator: estree::UpdateOperator::MinusMinus,
            prefix: false,
            argument: box ex.1,
        }),
    ))
}

fn UpdateExpression3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::DoubleMinus)?;
    let ex = UnaryExpression(tok.0)?;

    Ok((
        ex.0,
        node::Expression::UpdateExpression(node::UpdateExpression {
            operator: estree::UpdateOperator::MinusMinus,
            prefix: true,
            argument: box ex.1,
        }),
    ))
}

fn UpdateExpression4(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::DoublePlus)?;
    let ex = UnaryExpression(tok.0)?;

    Ok((
        ex.0,
        node::Expression::UpdateExpression(node::UpdateExpression {
            operator: estree::UpdateOperator::PlusPlus,
            prefix: true,
            argument: box ex.1,
        }),
    ))
}

named!(pub LeftHandSideExpression<Input<InputWrapper>, node::Expression>, alt!(
    NewExpression /*|
    CallExpression*/
));

named!(pub NewExpression<Input<InputWrapper>, node::Expression>, alt!(
    MemberExpression |
    NewExpression1
));

fn NewExpression1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::KNew)?;
    NewExpression(tok.0)
}

named!(pub MemberExpression<Input<InputWrapper>, node::Expression>, alt!(
    PrimaryExpression
));

named!(pub PrimaryExpression<Input<InputWrapper>, node::Expression>, alt!(
    IdentifierReference |
    Literal |
    ThisExpression |
    ArrayLiteral |
    ObjectLiteral |
    FunctionExpression
));

fn FunctionExpression(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let func = is_token!(tokens, Token::KFunction)?;
    let ident = BindingIdentifier(func.0);
    let bracket = is_token!(either(&ident, func.0), Token::LRound)?;
    let params = FormalParameters(bracket.0)?;
    let bracket = is_token!(params.0, Token::RRound)?;
    let bracket = is_token!(bracket.0, Token::LBrace)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RBrace)?;
    Ok((
        bracket.0,
        node::Expression::FunctionExpression(node::FunctionExpression {
            id: unwrap_or_none(ident),
            params: params.1,
            body: node::FunctionBody {
                body: body
                    .1
                    .into_iter()
                    .map(|i| node::FunctionBodyEnum::Statement(i))
                    .collect(),
            },
            generator: false,
            async_: false,
        }),
    ))
}

named!(pub ObjectLiteral<Input<InputWrapper>, node::Expression>, alt!(
    ObjectLiteral1 |
    ObjectLiteral2 |
    ObjectLiteral3
));

fn ObjectLiteral1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let bracket = is_token!(bracket.0, Token::RBrace)?;
    Ok((
        bracket.0,
        node::Expression::ObjectExpression(node::ObjectExpression { properties: vec![] }),
    ))
}

fn ObjectLiteral2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let list = separated_nonempty_list!(bracket.0, is_token!(Token::Comma), PropertyDefinition)?;
    let bracket = is_token!(list.0, Token::RBrace)?;
    Ok((
        bracket.0,
        node::Expression::ObjectExpression(node::ObjectExpression { properties: list.1 }),
    ))
}

fn ObjectLiteral3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LBrace)?;
    let list = separated_nonempty_list!(bracket.0, is_token!(Token::Comma), PropertyDefinition)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let bracket = is_token!(list.0, Token::RBrace)?;
    Ok((
        bracket.0,
        node::Expression::ObjectExpression(node::ObjectExpression { properties: list.1 }),
    ))
}

named!(pub PropertyDefinition<Input<InputWrapper>, node::ObjectExpressionProperty>, alt!(
    PropertyDefinition1 |
    PropertyDefinition2 |
    PropertyDefinition3
));

fn PropertyDefinition3(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectExpressionProperty> {
    let ident = IdentifierReference(tokens)?;
    Ok((
        ident.0,
        node::ObjectExpressionProperty::Property(node::property {
            key: ident.1.clone(),
            shorthand: true,
            value: ident.1,
            method: false,
            kind: estree::PropertyKind::Init,
            computed: false,
        }),
    ))
}

fn PropertyDefinition2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectExpressionProperty> {
    let name = PropertyName(tokens)?;
    let colon = is_token!(name.0, Token::Colon)?;
    let assign = AssignmentExpression(colon.0)?;
    Ok((
        assign.0,
        node::ObjectExpressionProperty::Property(node::property {
            key: name.1,
            shorthand: false,
            value: assign.1,
            method: false,
            kind: estree::PropertyKind::Init,
            computed: false,
        }),
    ))
}

fn PropertyDefinition1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ObjectExpressionProperty> {
    let method = MethodDefinition(tokens)?;
    Ok((
        method.0,
        node::ObjectExpressionProperty::Property(node::property {
            key: method.1.key,
            shorthand: false,
            value: node::Expression::FunctionExpression(method.1.value),
            method: true,
            kind: estree::PropertyKind::Init,
            computed: false,
        }),
    ))
}

named!(pub ArrayLiteral<Input<InputWrapper>, node::Expression>, alt!(
    ArrayLiteral1 |
    ArrayLiteral2 |
    ArrayLiteral3
));

fn ArrayLiteral1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let elision = Elision(bracket.0);
    let bracket = is_token!(either(&elision, bracket.0), Token::RSquare)?;
    Ok((
        bracket.0,
        node::Expression::ArrayExpression(node::ArrayExpression { elements: vec![] }),
    ))
}

fn ArrayLiteral2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let list = many0!(bracket.0, ElementList)?;
    let bracket = is_token!(list.0, Token::RSquare)?;
    Ok((
        bracket.0,
        node::Expression::ArrayExpression(node::ArrayExpression { elements: list.1 }),
    ))
}

fn ArrayLiteral3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LSquare)?;
    let list = many0!(bracket.0, ElementList)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let elision = Elision(comma.0);
    let bracket = is_token!(either(&elision, comma.0), Token::RSquare)?;
    Ok((
        bracket.0,
        node::Expression::ArrayExpression(node::ArrayExpression { elements: list.1 }),
    ))
}

named!(pub ElementList<Input<InputWrapper>, Option<node::ArrayExpressionElement>>, alt!(
    ElementList1 |
    ElementList2
));

fn ElementList1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Option<node::ArrayExpressionElement>> {
    let elision = Elision(tokens);
    let assign = AssignmentExpression(either(&elision, tokens))?;
    Ok((
        assign.0,
        Some(node::ArrayExpressionElement::Expression(assign.1)),
    ))
}

fn ElementList2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Option<node::ArrayExpressionElement>> {
    let elision = Elision(tokens);
    let spread = SpreadElement(either(&elision, tokens))?;
    Ok((
        spread.0,
        Some(node::ArrayExpressionElement::SpreadElement(spread.1)),
    ))
}

fn SpreadElement(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::SpreadElement> {
    let dots = is_token!(tokens, Token::TripleDot)?;
    let assign = AssignmentExpression(dots.0)?;
    Ok((assign.0, node::SpreadElement { argument: assign.1 }))
}

fn ThisExpression(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let this = is_token!(tokens, Token::KThis)?;
    Ok((
        this.0,
        node::Expression::ThisExpression(node::ThisExpression {}),
    ))
}

named!(pub IdentifierReference<Input<InputWrapper>, node::Expression>, alt!(
    IdentifierName
));

named!(pub Literal<Input<InputWrapper>, node::Expression>, alt!(
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

    test!(
        simple_var,
        r#"var a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.id {
                        node::Pattern::Identifier(ref ident) => {
                            assert_eq!(ident.name, "a");
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_assignment_number,
        r#"var a = 1;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.init.as_ref().unwrap() {
                        node::Expression::NumberLiteral(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_assignment_string,
        r#"var a = "a";"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.init.as_ref().unwrap() {
                        node::Expression::StringLiteral(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        var_has_correct_kind,
        r#"var a = true;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_assignment_bool,
        r#"var a = true;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.init.as_ref().unwrap() {
                        node::Expression::BooleanLiteral(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_assignment_null,
        r#"var a = null;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.init.as_ref().unwrap() {
                        node::Expression::NullLiteral(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_assignment_ident,
        r#"var a = b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 1);
                    let ref decl = decl.get_declarations()[0];
                    match decl.init.as_ref().unwrap() {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        var_multiple,
        r#"var a, b, c;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 3);
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        var_multiple_with_init,
        r#"var a = 1, b = 2;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.declarations.len(), 2);
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.init.as_ref().unwrap() {
                        node::Expression::NumberLiteral(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_simple,
        r#"var {a} = b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 1);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    assert_eq!(assignment.get_shorthand(), true);
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_multiple_prop,
        r#"var {a, b, c} = d;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 3);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    assert_eq!(assignment.get_shorthand(), true);
                                }
                                _ => panic!(),
                            }
                            match arr.properties[2] {
                                node::ObjectPatternProperty::AssignmentProperty(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_rest,
        r#"var {...rest} = b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 1);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::RestElement(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_default_value,
        r#"var {a = true} = b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 1);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    match assignment.key {
                                        node::Expression::Identifier(_) => {}
                                        _ => panic!(),
                                    };
                                    match assignment.value {
                                        node::Pattern::AssignmentPattern(
                                            node::AssignmentPattern {
                                                right: box node::Expression::BooleanLiteral(_),
                                                ..
                                            },
                                        ) => {}
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_default_values,
        r#"var {a = false, a = false} = c;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 2);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    match assignment.key {
                                        node::Expression::Identifier(_) => {}
                                        _ => panic!(),
                                    };
                                    match assignment.value {
                                        node::Pattern::AssignmentPattern(
                                            node::AssignmentPattern {
                                                right: box node::Expression::BooleanLiteral(_),
                                                ..
                                            },
                                        ) => {}
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_complex_key,
        r#"var {[a]: a} = c;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 1);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    match assignment.key {
                                        node::Expression::Identifier(_) => {}
                                        _ => panic!(),
                                    };
                                    match assignment.value {
                                        node::Pattern::Identifier(_) => {}
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_nested_objects,
        r#"var {a: {b, d}} = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 1);
                            match arr.properties[0] {
                                node::ObjectPatternProperty::AssignmentProperty(ref assignment) => {
                                    match assignment.key {
                                        node::Expression::Identifier(_) => {}
                                        _ => panic!(),
                                    };
                                    match assignment.value {
                                        node::Pattern::ObjectPattern(_) => {}
                                        _ => panic!(),
                                    };
                                }
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_destruct_assignment_empty,
        r#"var {} = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ObjectPattern(ref arr) => {
                            assert_eq!(arr.properties.len(), 0);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_empty,
        r#"var [] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 0);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_empty_elision,
        r#"var [,,] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 2);
                            assert!(arr.elements[0].is_none());
                            assert!(arr.elements[1].is_none());
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_rest,
        r#"var [,,...rest] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 3);
                            match arr.elements[2].as_ref().unwrap() {
                                node::Pattern::RestElement(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_element,
        r#"var [,,a] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 3);
                            match arr.elements[2].as_ref().unwrap() {
                                node::Pattern::Identifier(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_elements,
        r#"var [, ,a, b] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 4);
                            match arr.elements[2].as_ref().unwrap() {
                                node::Pattern::Identifier(_) => {}
                                _ => panic!(),
                            }
                            match arr.elements[3].as_ref().unwrap() {
                                node::Pattern::Identifier(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_element_with_rest,
        r#"var [, ,a,, ...b] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref decl = decl.get_declarations()[0];
                    assert_eq!(decl.get_init().is_some(), true);
                    match decl.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 5);
                            match arr.elements[4].as_ref().unwrap() {
                                node::Pattern::RestElement(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_destruct_element_elision_end,
        r#"var [, ,a,,,b] = a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    let ref var = decl.get_declarations()[0];
                    match var.id {
                        node::Pattern::ArrayPattern(ref arr) => {
                            assert_eq!(arr.elements.len(), 6);
                            assert!(arr.elements[2].is_some());
                            assert!(arr.elements[4].is_none());
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        empty_block,
        r#"{}"#,
        {
            match decl {
                node::Statement::BlockStatement(decl) => {
                    assert_eq!(decl.body.len(), 0);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        simple_block,
        r#"{var a;}"#,
        {
            match decl {
                node::Statement::BlockStatement(decl) => {
                    assert_eq!(decl.body.len(), 1);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        empty_statement,
        r#";"#,
        {
            match decl {
                node::Statement::EmptyStatement(decl) => {}
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        expression_statement_simple,
        r#"a;"#,
        {
            match decl {
                node::Statement::ExpressionStatement(decl) => {
                    match *decl.expression {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        expression_statement_complex,
        r#"a, b;"#,
        {
            match decl {
                node::Statement::ExpressionStatement(decl) => {
                    match *decl.expression {
                        node::Expression::SequenceExpression(_) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        if_statement_simple,
        r#"if (a) {}"#,
        {
            match decl {
                node::Statement::IfStatement(decl) => {
                    match decl.test {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    };
                    match *decl.consequent {
                        node::Statement::BlockStatement(_) => {}
                        _ => panic!(),
                    };
                    assert_eq!(decl.alternate.is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        if_statement_with_else,
        r#"if (a) {} else {b;}"#,
        {
            match decl {
                node::Statement::IfStatement(decl) => {
                    match decl.test {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    };
                    match *decl.consequent {
                        node::Statement::BlockStatement(_) => {}
                        _ => panic!(),
                    };
                    match **decl.alternate.as_ref().unwrap() {
                        node::Statement::BlockStatement(_) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        do_statement,
        r#"do ; while (a);"#,
        {
            match decl {
                node::Statement::DoWhileStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match *decl.test {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        while_statement,
        r#"while (a);"#,
        {
            match decl {
                node::Statement::WhileStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.test {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        for_statement_with_expresions,
        r#"for(a;a;a);"#,
        {
            match decl {
                node::Statement::ForStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    assert!(decl.init.is_some());
                    assert!(decl.test.is_some());
                    assert!(decl.update.is_some());
                    match decl.test.as_ref().unwrap() {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    match decl.update.as_ref().unwrap() {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        for_statement_empty,
        r#"for(;;);"#,
        {
            match decl {
                node::Statement::ForStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    assert!(decl.init.is_none());
                    assert!(decl.test.is_none());
                    assert!(decl.update.is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        for_statement_var,
        r#"for(var a = 1;;);"#,
        {
            match decl {
                node::Statement::ForStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    assert!(decl.init.is_some());
                    assert!(decl.test.is_none());
                    assert!(decl.update.is_none());
                    match decl.init.as_ref().unwrap() {
                        node::ForStatementInit::VariableDeclaration(decl) => {
                            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                            assert_eq!(decl.get_declarations().len(), 1);
                        }
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        for_statement_var_multiple,
        r#"for(var a = 1, b = 2;;);"#,
        {
            match decl {
                node::Statement::ForStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    assert!(decl.init.is_some());
                    assert!(decl.test.is_none());
                    assert!(decl.update.is_none());
                    match decl.init.as_ref().unwrap() {
                        node::ForStatementInit::VariableDeclaration(decl) => {
                            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                            assert_eq!(decl.get_declarations().len(), 2);
                        }
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        for_statement_let,
        r#"for(let a = 1;;);"#,
        {
            match decl {
                node::Statement::ForStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    assert!(decl.init.is_some());
                    assert!(decl.test.is_none());
                    assert!(decl.update.is_none());
                    match decl.init.as_ref().unwrap() {
                        node::ForStatementInit::VariableDeclaration(decl) => {
                            assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
                        }
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forin_statement_assignment,
        r#"for(a in b);"#,
        {
            match decl {
                node::Statement::ForInStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::Pattern(node::Pattern::Identifier(_)) => {}
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forin_statement_var,
        r#"for(var a in b);"#,
        {
            match decl {
                node::Statement::ForInStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forin_statement_const,
        r#"for(const a in b);"#,
        {
            match decl {
                node::Statement::ForInStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forof_statement_assignment,
        r#"for(a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::Pattern(node::Pattern::Identifier(_)) => {}
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forof_statement_var,
        r#"for(var a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forof_statement_const,
        r#"for(const a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forofawait_statement_assignment,
        r#"for await(a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::Pattern(node::Pattern::Identifier(_)) => {}
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forofawait_statement_var,
        r#"for await(var a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Var);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        forofawait_statement_const,
        r#"for await(const a of b);"#,
        {
            match decl {
                node::Statement::ForOfStatement(decl) => {
                    match *decl.body {
                        node::Statement::EmptyStatement(_) => {}
                        _ => panic!(),
                    };
                    match decl.left {
                        node::ForInStatementLeft::VariableDeclaration(ref left) => {
                            assert_eq!(left.get_kind(), &estree::VariableDeclarationKind::Const);
                        }
                        _ => panic!(),
                    };
                    match decl.right {
                        node::Expression::Identifier(_) => {}
                        _ => panic!(),
                    }
                    assert_eq!(decl.await_, true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        continue_statement,
        r#"continue;"#,
        {
            match decl {
                node::Statement::ContinueStatement(decl) => {
                    assert_eq!(decl.label.is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        continue_statement_with_label,
        r#"continue aa;"#,
        {
            match decl {
                node::Statement::ContinueStatement(decl) => {
                    assert_eq!(decl.label.as_ref().unwrap().name, "aa")
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        break_statement,
        r#"break;"#,
        {
            match decl {
                node::Statement::BreakStatement(decl) => {
                    assert_eq!(decl.label.is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        break_statement_with_label,
        r#"break aa;"#,
        {
            match decl {
                node::Statement::BreakStatement(decl) => {
                    assert_eq!(decl.label.as_ref().unwrap().name, "aa")
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        return_statement,
        r#"return;"#,
        {
            match decl {
                node::Statement::ReturnStatement(decl) => {
                    assert_eq!(decl.argument.is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        return_statement_with_expression,
        r#"return aa;"#,
        {
            match decl {
                node::Statement::ReturnStatement(decl) => match decl.argument.as_ref().unwrap() {
                    node::Expression::Identifier(ident) => assert_eq!(ident.name, "aa"),
                    _ => panic!(),
                },
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        with_statement,
        r#"with (a) {};"#,
        {
            match decl {
                node::Statement::WithStatement(decl) => {
                    match decl.object {
                        node::Expression::Identifier(node::Identifier { .. }) => {}
                        _ => panic!(),
                    };

                    match *decl.body {
                        node::Statement::BlockStatement(node::BlockStatement { .. }) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        labeled_statement,
        r#"a: {}"#,
        {
            match decl {
                node::Statement::LabeledStatement(decl) => {
                    assert_eq!(decl.get_label().get_name(), "a");
                    match *decl.body {
                        node::Statement::BlockStatement(node::BlockStatement { .. }) => {}
                        _ => panic!(),
                    };
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        try_statement_catch,
        r#"try {} catch (a) {}"#,
        {
            match decl {
                node::Statement::TryStatement(decl) => {
                    let ref param = decl.get_handler().as_ref().unwrap().param;
                    match param {
                        node::Pattern::Identifier(ident) => assert_eq!(ident.get_name(), "a"),
                        _ => panic!(),
                    }
                    assert!(decl.get_finalizer().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        try_statement_finally,
        r#"try {} finally {}"#,
        {
            match decl {
                node::Statement::TryStatement(decl) => {
                    assert!(decl.get_handler().is_none());
                    decl.get_finalizer().as_ref().unwrap();
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        try_statement_catch_finally,
        r#"try {} catch (a) {} finally {}"#,
        {
            match decl {
                node::Statement::TryStatement(decl) => {
                    let finalizer = decl.get_finalizer().as_ref().unwrap();
                    let ref param = decl.get_handler().as_ref().unwrap().param;
                    match param {
                        node::Pattern::Identifier(ident) => assert_eq!(ident.get_name(), "a"),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        debugger_statement,
        r#"debugger;"#,
        {
            match decl {
                node::Statement::DebuggerStatement(node::DebuggerStatement {}) => {}
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        function_declaration_simple,
        r#"function a() {}"#,
        {
            match decl {
                node::Statement::FunctionDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name().clone(), "a");
                    assert_eq!(decl.params.len(), 0);
                    assert_eq!(decl.generator, false);
                    assert_eq!(decl.async_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        function_declaration_simple_arg,
        r#"function a(a, b) {}"#,
        {
            match decl {
                node::Statement::FunctionDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "a");
                    assert_eq!(decl.params.len(), 2);
                    assert_eq!(decl.generator, false);
                    assert_eq!(decl.async_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        function_declaration_rest,
        r#"function a(...b) {}"#,
        {
            match decl {
                node::Statement::FunctionDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "a");
                    assert_eq!(decl.params.len(), 1);
                    assert_eq!(decl.generator, false);
                    assert_eq!(decl.async_, false);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        class_declaration_simple,
        r#"class b {}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        class_declaration_inheritance,
        r#"class b extend a {}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    match decl.super_class.as_ref().unwrap() {
                        node::Expression::Identifier(ident) => assert_eq!(ident.name, "a"),
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        method_declaration_simple,
        r#"class b {a() {}}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                    let body = decl.get_body().get_body();
                    assert_eq!(body.len(), 1);
                    match body[0].key {
                        node::Expression::Identifier(ref ident) => {
                            assert_eq!(ident.get_name(), "a")
                        }
                        _ => panic!(),
                    }

                    assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
                    assert_eq!(body[0].get_static(), false);
                    let value = body[0].get_value();
                    assert_eq!(value.get_params().len(), 0);
                    assert!(value.get_id().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        method_declaration_params,
        r#"class b {a(b,c) {}}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                    let body = decl.get_body().get_body();
                    assert_eq!(body.len(), 1);
                    match body[0].key {
                        node::Expression::Identifier(ref ident) => {
                            assert_eq!(ident.get_name(), "a")
                        }
                        _ => panic!(),
                    }

                    assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
                    assert_eq!(body[0].get_static(), false);
                    let value = body[0].get_value();
                    assert_eq!(value.get_params().len(), 2);
                    assert!(value.get_id().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        method_declaration_set,
        r#"class b {set a(b) {}}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                    let body = decl.get_body().get_body();
                    assert_eq!(body.len(), 1);
                    match body[0].key {
                        node::Expression::Identifier(ref ident) => {
                            assert_eq!(ident.get_name(), "a")
                        }
                        _ => panic!(),
                    }

                    assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Set);
                    assert_eq!(body[0].get_static(), false);
                    let value = body[0].get_value();
                    assert_eq!(value.get_params().len(), 1);
                    assert!(value.get_id().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        method_declaration_get,
        r#"class b {get a() {}}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                    let body = decl.get_body().get_body();
                    assert_eq!(body.len(), 1);
                    match body[0].key {
                        node::Expression::Identifier(ref ident) => {
                            assert_eq!(ident.get_name(), "a")
                        }
                        _ => panic!(),
                    }

                    assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Get);
                    assert_eq!(body[0].get_static(), false);
                    let value = body[0].get_value();
                    assert_eq!(value.get_params().len(), 0);
                    assert!(value.get_id().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        method_declaration_static,
        r#"class b {static a() {}}"#,
        {
            match decl {
                node::Statement::ClassDeclaration(decl) => {
                    assert_eq!(decl.id.as_ref().unwrap().get_name(), "b");
                    assert_eq!(decl.super_class.is_none(), true);
                    let body = decl.get_body().get_body();
                    assert_eq!(body.len(), 1);
                    match body[0].key {
                        node::Expression::Identifier(ref ident) => {
                            assert_eq!(ident.get_name(), "a")
                        }
                        _ => panic!(),
                    }
                    assert_eq!(body[0].get_kind(), &estree::MethodDefinitionKind::Method);
                    assert_eq!(body[0].get_static(), true);
                    let value = body[0].get_value();
                    assert_eq!(value.get_params().len(), 0);
                    assert!(value.get_id().is_none());
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        lexical_declaration_let_simple,
        r#"let a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        lexical_declaration_const_init,
        r#"const a = 1;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Const);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        lexical_declaration_let_multiple,
        r#"let a = 1, b = 2;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Let);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 2);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        this_expression,
        r#"var a = this;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ThisExpression(node::ThisExpression {}) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_empty,
        r#"var a = [];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ArrayExpression(expr) => {
                            assert_eq!(expr.elements.len(), 0);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_simple,
        r#"var a = [1];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ArrayExpression(expr) => {
                            assert_eq!(expr.elements.len(), 1);
                            match expr.elements[0].as_ref().unwrap() {
                                node::ArrayExpressionElement::Expression(
                                    node::Expression::NumberLiteral(num),
                                ) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_multiple,
        r#"var a = [1, 2,,,];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ArrayExpression(expr) => {
                            assert_eq!(expr.elements.len(), 2);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_elision,
        r#"var a = [1, 2,,,];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ArrayExpression(expr) => {
                            assert_eq!(expr.elements.len(), 2);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_elision_empty,
        r#"var a = [,,,];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        array_literal_spread,
        r#"var a = [1, ...a];"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ArrayExpression(expr) => {
                            assert_eq!(expr.elements.len(), 2);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_literal_empty,
        r#"var a = {};"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ObjectExpression(expr) => {
                            assert_eq!(expr.properties.len(), 0);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_literal_simple,
        r#"var a = {b: 1};"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ObjectExpression(expr) => {
                            assert_eq!(expr.properties.len(), 1);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_literal_method,
        r#"var a = {b() {}};"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ObjectExpression(expr) => {
                            assert_eq!(expr.properties.len(), 1);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        object_literal_multiple_colon,
        r#"var a = {1: 1, "a":2, 'b': 3, c: 4};"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::ObjectExpression(expr) => {
                            assert_eq!(expr.properties.len(), 4);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        logical_or_expression,
        r#"var a = a || b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::LogicalExpression(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        logical_or_expression_multiple,
        r#"var a = a || b || c;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::LogicalExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::LogicalOperator::Or);
                            match *expr.right {
                                node::Expression::LogicalExpression(_) => {}
                                _ => panic!(),
                            }
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        logical_and_expression,
        r#"var a = a && b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::LogicalExpression(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        logical_and_expression_multiple,
        r#"var a = a && b && c;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::LogicalExpression(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        logical_and_or_expression_multiple,
        r#"var a = a || c && b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::LogicalExpression(_) => {}
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_equal,
        r#"var a = a == b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::EqualEqual);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_not_equal,
        r#"var a = a != b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::ExclamationEqual);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_equal_equal,
        r#"var a = a === b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::EqualEqualEqual);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_not_equal_equal,
        r#"var a = a !== b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(
                                expr.operator,
                                estree::BinaryOperator::ExclamationEqualEqual
                            );
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_left_shift,
        r#"var a = a << b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::LessLess);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_right_shift,
        r#"var a = a >> b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::MoreMore);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_zero_fill_right_shift,
        r#"var a = a >>> b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::MoreMoreMore);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_plus,
        r#"var a = a + b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::Plus);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_minus,
        r#"var a = a - b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::Minus);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_multiply,
        r#"var a = a * b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::Times);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_mod,
        r#"var a = a % b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::Percent);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_divide,
        r#"var a = a / b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::Slash);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        binary_operator_power,
        r#"var a = a ** b;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::BinaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::BinaryOperator::StarStar);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_delete,
        r#"var a = delete a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Delete);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_void,
        r#"var a = void a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Void);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_typeof,
        r#"var a = typeof a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Typeof);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_minus,
        r#"var a = -a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Minus);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_plus,
        r#"var a = +a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Plus);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_tilde,
        r#"var a = ~a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Tilde);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        unary_operator_exclamation,
        r#"var a = !a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UnaryExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UnaryOperator::Exclamation);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        update_expression_post_plusplus,
        r#"var a = a++;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UpdateExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UpdateOperator::PlusPlus);
                            assert_eq!(expr.prefix, false);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        update_expression_post_minusminus,
        r#"var a = a--;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UpdateExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UpdateOperator::MinusMinus);
                            assert_eq!(expr.prefix, false);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        update_expression_prefix_minusminus,
        r#"var a = --a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UpdateExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UpdateOperator::MinusMinus);
                            assert_eq!(expr.prefix, true);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );

    test!(
        update_expression_prefix_plusplus,
        r#"var a = ++a;"#,
        {
            match decl {
                node::Statement::VariableDeclaration(decl) => {
                    assert_eq!(decl.get_kind(), &estree::VariableDeclarationKind::Var);
                    let decls = decl.get_declarations();
                    assert_eq!(decls.len(), 1);
                    match decls[0].get_init().as_ref().unwrap() {
                        node::Expression::UpdateExpression(ref expr) => {
                            assert_eq!(expr.operator, estree::UpdateOperator::PlusPlus);
                            assert_eq!(expr.prefix, true);
                        }
                        _ => panic!(),
                    }
                }
                _ => panic!(),
            }
        },
        decl
    );
}
