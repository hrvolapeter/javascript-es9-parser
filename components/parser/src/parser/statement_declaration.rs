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
        expression::{
            AssignmentExpression, Expression, ExpressionStatement, Initializer,
            LeftHandSideExpression, PropertyName,
        },
        input_wrapper::InputWrapper,
        macros::{either, unwrap_or_none},
        node::{self, Pattern, Statement},
    },
};
use nom::{
    types::{Input, *},
    IResult, *,
};

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

pub fn BindingIdentifier(
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

pub fn Elision(
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

pub fn AssignmentOperator(
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

pub fn FunctionBody(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Statement>> {
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
}
