use crate::{
    estree,
    input_wrapper::InputWrapper,
    javascript_lexer::token::Token,
    macros::{either, unwrap_or_none},
    node,
    statement_declaration::{
        AssignmentOperator, BindingIdentifier, BindingPattern, Elision, FormalParameters,
        FunctionBody, MethodDefinition,
    },
};
use nom::{types::Input, IResult, *};

pub fn ExpressionStatement(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Statement> {
    not!(tokens, is_token!(Token::LCurly))?;
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

pub fn Expression(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
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

pub fn Initializer(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let eq = is_token!(tokens, Token::Assign)?;
    AssignmentExpression(eq.0)
}

named!(pub ArrowParameters<Input<InputWrapper>, Vec<node::Pattern>>, alt!(
    do_parse!(
        res: BindingIdentifier >>
        (vec![node::Pattern::Identifier(res)])) |
    CoverParenthesizedExpressionAndArrowParameterList
));

fn ArrowFunction(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let params = ArrowParameters(tokens)?;
    let arrow = is_token!(params.0, Token::AssignBigger)?;
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

named!(pub ConciseBody<Input<InputWrapper>, node::ArrowFunctionExpressionBody>, alt!(
    ConciseBody1 |
    ConciseBody2
));

fn ConciseBody1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrowFunctionExpressionBody> {
    not!(tokens, is_token!(Token::LCurly))?;
    let assignment = AssignmentExpression(tokens)?;

    Ok((
        assignment.0,
        node::ArrowFunctionExpressionBody::Expression(box assignment.1),
    ))
}

fn ConciseBody2(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::ArrowFunctionExpressionBody> {
    let bracket = is_token!(tokens, Token::LCurly)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RCurly)?;

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
    let comma = is_token!(expr.0, Token::Comma)?;
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
    let comma = is_token!(expr.0, Token::Comma)?;
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
    let tok = is_token!(skipped.0, Token::DoubleOr)?;
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
    let tok = is_token!(skipped.0, Token::DoubleAnd)?;
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
    do_parse!(
        relat : RelationalExpression >>
        res: alt!(
            call!(EqualityExpression1, relat.clone()) |
            call!(EqualityExpression2, relat.clone()) |
            call!(EqualityExpression3, relat.clone()) |
            call!(EqualityExpression4, relat)
        ) >>
        (res)) |
    RelationalExpression
));

fn EqualityExpression1(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::DoubleAssign)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::EqualEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression2(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::ExclamationAssign)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::ExclamationEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression3(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::TripleAssign)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::EqualEqualEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn EqualityExpression4(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::ExclamationDoubleAssign)?;
    let and = EqualityExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::ExclamationEqualEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

named!(pub RelationalExpression<Input<InputWrapper>, node::Expression>, alt!(
    do_parse!(
        shift : ShiftExpression >>
        res: alt!(
            call!(RelationalExpression1, shift.clone()) |
            call!(RelationalExpression2, shift.clone()) |
            call!(RelationalExpression3, shift.clone()) |
            call!(RelationalExpression4, shift)
        ) >>
        (res)) |
    ShiftExpression
));

fn RelationalExpression1(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Lesser)?;
    let and = RelationalExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::Less,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn RelationalExpression2(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::Bigger)?;
    let and = RelationalExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::More,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn RelationalExpression3(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::LessEqual)?;
    let and = RelationalExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::LessEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

fn RelationalExpression4(
    tokens: Input<InputWrapper>,
    expr: node::Expression,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::BiggerEqual)?;
    let and = RelationalExpression(tok.0)?;
    Ok((
        and.0,
        node::Expression::BinaryExpression(node::BinaryExpression {
            operator: estree::BinaryOperator::MoreEqual,
            left: box expr,
            right: box and.1,
        }),
    ))
}

named!(pub ShiftExpression<Input<InputWrapper>, node::Expression>, alt!(
    ShiftExpression1 |
    ShiftExpression2 |
    ShiftExpression3 |
    AdditiveExpression
));

fn ShiftExpression1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let skipped = take!(tokens.clone(), 1)?;
    let tok = is_token!(skipped.0, Token::DoubleLesser)?;
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
    let tok = is_token!(skipped.0, Token::DoubleBigger)?;
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
    let tok = is_token!(skipped.0, Token::TripleBigger)?;
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
    let tok = is_token!(skipped.0, Token::Star)?;
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
    let tok = is_token!(skipped.0, Token::Percent)?;
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
    let tok = is_token!(skipped.0, Token::DoubleStar)?;
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
    CallExpression |
    NewExpression
));

named!(pub CallExpression<Input<InputWrapper>, node::Expression>, alt!(
    NewExpressionCoverCallExpressionAndAsyncArrowHead
));

fn NewExpressionCoverCallExpressionAndAsyncArrowHead(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let member = MemberExpression(tokens)?;
    let left = is_token!(member.0, Token::LRound)?;
    let args = ArgumentList(left.0)?;
    let right = is_token!(args.0, Token::RRound)?;
    Ok((
        right.0,
        node::Expression::CallExpression(node::CallExpression {
            callee: box member.1,
            arguments: args.1,
        }),
    ))
}

fn ArgumentList(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, Vec<node::Expression>> {
    separated_list!(tokens, is_token!(Token::Comma), AssignmentExpression)
}

named!(pub NewExpression<Input<InputWrapper>, node::Expression>, alt!(
    MemberExpression |
    NewExpression1
));

fn NewExpression1(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let tok = is_token!(tokens, Token::KNew)?;
    NewExpression(tok.0)
}

named!(pub MemberExpression<Input<InputWrapper>, node::Expression>, alt!(
    MemberExpression1 |
    PrimaryExpression
));

fn MemberExpression1(
    tokens: Input<InputWrapper>,
) -> IResult<Input<InputWrapper>, node::Expression> {
    let member = take!(tokens, 1)?;
    let dot = is_token!(member.0, Token::Dot)?;
    let ident = BindingIdentifier(dot.0)?;
    Ok((
        ident.0,
        node::Expression::MemberExpression(node::MemberExpression {
            computed: false,
            object: box MemberExpression(member.1)?.1,
            property: ident.1,
        }),
    ))
}

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
    let bracket = is_token!(bracket.0, Token::LCurly)?;
    let body = FunctionBody(bracket.0)?;
    let bracket = is_token!(body.0, Token::RCurly)?;
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
    let bracket = is_token!(tokens, Token::LCurly)?;
    let bracket = is_token!(bracket.0, Token::RCurly)?;
    Ok((
        bracket.0,
        node::Expression::ObjectExpression(node::ObjectExpression { properties: vec![] }),
    ))
}

fn ObjectLiteral2(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LCurly)?;
    let list = separated_nonempty_list!(bracket.0, is_token!(Token::Comma), PropertyDefinition)?;
    let bracket = is_token!(list.0, Token::RCurly)?;
    Ok((
        bracket.0,
        node::Expression::ObjectExpression(node::ObjectExpression { properties: list.1 }),
    ))
}

fn ObjectLiteral3(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let bracket = is_token!(tokens, Token::LCurly)?;
    let list = separated_nonempty_list!(bracket.0, is_token!(Token::Comma), PropertyDefinition)?;
    let comma = is_token!(list.0, Token::Comma)?;
    let bracket = is_token!(comma.0, Token::RCurly)?;
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

named!(pub LiteralPropertyName<Input<InputWrapper>, node::Expression>, alt!(
    IdentifierName |
    StringLiteral |
    NumericLiteral
));

fn IdentifierName(tokens: Input<InputWrapper>) -> IResult<Input<InputWrapper>, node::Expression> {
    let identifier = take!(tokens, 1)?;
    if let Token::IdentifierName(ident) = &identifier.1.inner[0] {
        return Ok((
            identifier.0,
            node::Expression::Identifier(node::Identifier {
                name: ident.clone(),
            }),
        ));
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

#[cfg(test)]
mod test {
    use super::*;
    use crate::{
        estree::{VariableDeclaration, *},
        javascript_lexer::Lexer,
        Parser,
    };

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
                                    node::Expression::NumberLiteral(_),
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
