use crate::{lexer::token::Number, parser::estree};
use std::convert::TryFrom;

pub enum Expression {}

// #[derive(Debug)]
// pub enum Literal {
//     StringLiteral,
//     BooleanLiteral,
//     NullLiteral,
//     NumberLiteral,
//     RegExpLiteral,
// }
// impl estree::Literal for Literal {}
// impl estree::Node for Literal {}
// impl estree::Expression for Literal {}

#[derive(Debug, Clone)]
pub struct StringLiteral(pub String);
impl estree::Literal for StringLiteral {}
impl estree::Node for StringLiteral {}
impl estree::Expression for StringLiteral {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone)]
pub struct BooleanLiteral(pub bool);
impl estree::Literal for BooleanLiteral {}
impl estree::Node for BooleanLiteral {}
impl estree::Expression for BooleanLiteral {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone)]
pub struct NullLiteral;
impl estree::Literal for NullLiteral {}
impl estree::Node for NullLiteral {}
impl estree::Expression for NullLiteral {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone)]
pub struct NumberLiteral(pub Number);
impl estree::Literal for NumberLiteral {}
impl estree::Node for NumberLiteral {}
impl estree::Expression for NumberLiteral {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug, Clone)]
pub struct Regex {}
impl estree::Literal for Regex {}
impl estree::Node for Regex {}
impl estree::Expression for Regex {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

impl From<VariableDeclaration> for estree::ProgramStatement {
    fn from(value: VariableDeclaration) -> Self {
        estree::ProgramStatement::VariableDeclaration(Box::new(value))
    }
}

#[derive(Debug)]
pub struct Program {
    pub sourceType: estree::ProgramSourceType,
    pub body: Vec<estree::ProgramBody>,
}

impl estree::Program for Program {
    fn get_body(&self) -> &Vec<estree::ProgramBody> {
        &self.body
    }

    fn get_source_type(&self) -> &estree::ProgramSourceType {
        &self.sourceType
    }
}

impl estree::Node for Program {}

#[derive(Debug)]
pub struct VariableDeclarator {
    pub id: Box<estree::Pattern>,
    pub init: Option<Box<estree::Expression>>,
}

impl Clone for VariableDeclarator {
    fn clone(&self) -> Self {
        Self {
            id: self.id.box_clone(),
            init: self.init.as_ref().map(|i| i.box_clone()),
        }
    }
}

impl VariableDeclarator {
    pub fn new(
        id: Box<estree::Pattern>,
        init: Option<Box<estree::Expression>>,
    ) -> Box<estree::VariableDeclarator> {
        Box::new(Self { id, init })
    }
}

impl estree::VariableDeclarator for VariableDeclarator {
    fn get_id(&self) -> &Box<estree::Pattern> {
        &self.id
    }

    fn get_init(&self) -> &Option<Box<estree::Expression>> {
        &self.init
    }

    fn box_clone(&self) -> Box<estree::VariableDeclarator> {
        box (*self).clone()
    }
}
impl estree::Node for VariableDeclarator {}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub kind: estree::VariableDeclarationKind,
    pub declarations: Vec<Box<estree::VariableDeclarator>>,
}

impl Clone for VariableDeclaration {
    fn clone(&self) -> Self {
        Self {
            kind: self.kind.clone(),
            declarations: self
                .declarations
                .iter()
                .map(|i| estree::VariableDeclarator::box_clone(&**i))
                .collect(),
        }
    }
}

impl estree::VariableDeclaration for VariableDeclaration {
    fn get_declarations(&self) -> &Vec<Box<estree::VariableDeclarator>> {
        &self.declarations
    }

    fn get_kind(&self) -> &estree::VariableDeclarationKind {
        &self.kind
    }

    fn box_clone(&self) -> Box<estree::VariableDeclaration> {
        box (*self).clone()
    }
}

impl estree::Declaration for VariableDeclaration {
    fn box_clone(&self) -> Box<estree::Declaration> {
        box (*self).clone()
    }
}

impl estree::Statement for VariableDeclaration {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for VariableDeclaration {}

#[derive(Debug, Clone)]
pub struct ObjectPattern {
    pub properties: Vec<estree::ObjectPatternProperty>,
}

impl estree::ObjectPattern for ObjectPattern {
    fn get_properties(&self) -> &Vec<estree::ObjectPatternProperty> {
        &self.properties
    }
}
impl estree::Pattern for ObjectPattern {
    fn box_clone(&self) -> Box<estree::Pattern> {
        Box::new((*self).clone())
    }
}
impl estree::Node for ObjectPattern {}

#[derive(Debug)]
pub struct ArrayPattern {
    pub elements: Vec<Option<Box<estree::Pattern>>>,
}

impl Clone for ArrayPattern {
    fn clone(&self) -> Self {
        Self {
            elements: self
                .elements
                .iter()
                .map(|b| b.as_ref().map(|s| s.box_clone()))
                .collect(),
        }
    }
}

impl estree::ArrayPattern for ArrayPattern {
    fn get_elements(&self) -> &Vec<Option<Box<estree::Pattern>>> {
        &self.elements
    }
}
impl estree::Pattern for ArrayPattern {
    fn box_clone(&self) -> Box<estree::Pattern> {
        Box::new((*self).clone())
    }
}
impl estree::Node for ArrayPattern {}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
}

impl estree::Identifier for Identifier {
    fn get_name(&self) -> &str {
        self.name.as_str()
    }

    fn box_clone(&self) -> Box<estree::Identifier> {
        box (*self).clone()
    }
}
impl estree::Pattern for Identifier {
    fn box_clone(&self) -> Box<estree::Pattern> {
        Box::new((*self).clone())
    }
}
impl estree::Node for Identifier {}
impl estree::Expression for Identifier {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier { name }
    }
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub properties: Vec<estree::ObjectExpressionProperty>,
}

impl estree::ObjectExpression for ObjectExpression {
    fn get_properties(&self) -> &Vec<estree::ObjectExpressionProperty> {
        &self.properties
    }
}
impl estree::Node for ObjectExpression {}
impl estree::Expression for ObjectExpression {
    fn box_clone(&self) -> Box<estree::Expression> {
        Box::new((*self).clone())
    }
}

#[derive(Debug)]
pub struct RestElement {
    pub argument: Box<estree::Pattern>,
}

impl Clone for RestElement {
    fn clone(&self) -> Self {
        Self {
            argument: self.argument.box_clone(),
        }
    }
}

impl estree::RestElement for RestElement {
    fn get_argument(&self) -> &Box<estree::Pattern> {
        &self.argument
    }

    fn box_clone(&self) -> Box<estree::RestElement> {
        Box::new((*self).clone())
    }
}
impl estree::Pattern for RestElement {
    fn box_clone(&self) -> Box<estree::Pattern> {
        Box::new((*self).clone())
    }
}
impl estree::Node for RestElement {}

#[derive(Debug)]
pub struct AssignmentProperty {
    pub key: Box<estree::Expression>,
    pub value: Box<estree::Pattern>,
    pub shorthand: bool,
}

impl Clone for AssignmentProperty {
    fn clone(&self) -> Self {
        Self {
            key: self.key.box_clone(),
            value: self.value.box_clone(),
            shorthand: self.shorthand,
        }
    }
}

impl estree::AssignmentProperty for AssignmentProperty {
    fn get_value(&self) -> &Box<estree::Pattern> {
        &self.value
    }

    fn box_clone(&self) -> Box<estree::AssignmentProperty> {
        Box::new((*self).clone())
    }
}

impl estree::Property for AssignmentProperty {
    fn get_key(&self) -> &Box<estree::Expression> {
        &self.key
    }

    fn get_value(&self) -> &Box<estree::Expression> {
        unreachable!();
    }

    fn get_kind(&self) -> String {
        format!("{:?}", <Self as estree::AssignmentProperty>::get_kind(self))
    }

    fn get_method(&self) -> bool {
        <Self as estree::AssignmentProperty>::get_method(self)
    }

    fn get_shorthand(&self) -> bool {
        self.shorthand
    }

    fn get_computed(&self) -> bool {
        false
    }

    fn box_clone(&self) -> Box<estree::Property> {
        Box::new((*self).clone())
    }
}
impl estree::Node for AssignmentProperty {}

#[derive(Debug)]
pub struct AssignmentPattern {
    pub left: Box<estree::Pattern>,
    pub right: Box<estree::Expression>,
}

impl Clone for AssignmentPattern {
    fn clone(&self) -> Self {
        Self {
            left: self.left.box_clone(),
            right: self.right.box_clone(),
        }
    }
}

impl estree::AssignmentPattern for AssignmentPattern {
    fn get_left(&self) -> &Box<estree::Pattern> {
        &self.left
    }

    fn get_right(&self) -> &Box<estree::Expression> {
        &self.right
    }
}
impl estree::Pattern for AssignmentPattern {
    fn box_clone(&self) -> Box<estree::Pattern> {
        Box::new((*self).clone())
    }
}
impl estree::Node for AssignmentPattern {}

#[derive(Debug)]
pub struct BlockStatement {
    pub body: Vec<Box<estree::Statement>>,
}

impl Clone for BlockStatement {
    fn clone(&self) -> Self {
        Self {
            body: self.body.iter().map(|i| i.box_clone()).collect(),
        }
    }
}

impl estree::BlockStatement for BlockStatement {
    fn get_body(&self) -> &Vec<Box<estree::Statement>> {
        &self.body
    }

    fn box_clone(&self) -> Box<estree::BlockStatement> {
        Box::new((*self).clone())
    }
}
impl estree::Statement for BlockStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for BlockStatement {}

#[derive(Debug, Clone)]
pub struct EmptyStatement {}

impl estree::EmptyStatement for EmptyStatement {}
impl estree::Statement for EmptyStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for EmptyStatement {}

#[derive(Debug)]
pub struct ArrowFunctionExpression {
    pub body: estree::ArrowFunctionExpressionBody,
    pub expression: bool,
    pub id: Option<Box<estree::Identifier>>,
    pub params: Vec<Box<estree::Pattern>>,
    pub generator: bool,
    pub async_: bool,
}

impl Clone for ArrowFunctionExpression {
    fn clone(&self) -> Self {
        Self {
            body: self.body.clone(),
            expression: self.expression,
            id: self
                .id
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
            params: self.params.iter().map(|i| i.box_clone()).collect(),
            generator: self.generator,
            async_: self.async_,
        }
    }
}

impl estree::ArrowFunctionExpression for ArrowFunctionExpression {
    fn get_body(&self) -> &estree::ArrowFunctionExpressionBody {
        &self.body
    }

    fn get_expression(&self) -> bool {
        self.expression
    }
}
impl estree::Expression for ArrowFunctionExpression {
    fn box_clone(&self) -> Box<estree::Expression> {
        box (*self).clone()
    }
}
impl estree::Function for ArrowFunctionExpression {
    fn get_id(&self) -> &Option<Box<estree::Identifier>> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Box<estree::Pattern>> {
        &self.params
    }

    fn get_body(&self) -> &Box<estree::FunctionBody> {
        unimplemented!();
    }

    fn get_generator(&self) -> bool {
        self.generator
    }

    fn get_async(&self) -> bool {
        self.async_
    }
}
impl estree::Node for ArrowFunctionExpression {}

#[derive(Debug)]
pub struct FunctionExpression {
    pub id: Option<Box<estree::Identifier>>,
    pub params: Vec<Box<estree::Pattern>>,
    pub body: Box<estree::FunctionBody>,
    pub generator: bool,
    pub async_: bool,
}

impl Clone for FunctionExpression {
    fn clone(&self) -> Self {
        Self {
            id: self
                .id
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
            params: self.params.iter().map(|i| i.box_clone()).collect(),
            body: estree::FunctionBody::box_clone(&*self.body),
            generator: self.generator,
            async_: self.async_,
        }
    }
}

impl estree::FunctionExpression for FunctionExpression {
    fn box_clone(&self) -> Box<estree::FunctionExpression> {
        box (*self).clone()
    }
}
impl estree::Function for FunctionExpression {
    fn get_id(&self) -> &Option<Box<estree::Identifier>> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Box<estree::Pattern>> {
        &self.params
    }

    fn get_body(&self) -> &Box<estree::FunctionBody> {
        &self.body
    }

    fn get_generator(&self) -> bool {
        self.generator
    }

    fn get_async(&self) -> bool {
        self.async_
    }
}
impl estree::Expression for FunctionExpression {
    fn box_clone(&self) -> Box<estree::Expression> {
        box (*self).clone()
    }
}
impl estree::Node for FunctionExpression {}

#[derive(Debug)]
pub struct FunctionBody {
    pub body: Vec<estree::FunctionBodyEnum>,
}
impl Clone for FunctionBody {
    fn clone(&self) -> Self {
        Self {
            body: self.body.iter().map(|i| i.clone()).collect(),
        }
    }
}

impl estree::FunctionBody for FunctionBody {
    fn get_body(&self) -> &Vec<estree::FunctionBodyEnum> {
        &self.body
    }

    fn box_clone(&self) -> Box<estree::FunctionBody> {
        box (*self).clone()
    }
}
impl estree::BlockStatement for FunctionBody {
    fn get_body(&self) -> &Vec<Box<estree::Statement>> {
        unimplemented!()
    }

    fn box_clone(&self) -> Box<estree::BlockStatement> {
        Box::new((*self).clone())
    }
}
impl estree::Statement for FunctionBody {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for FunctionBody {}

#[derive(Debug)]
pub struct AssignmentExpression {
    pub operator: estree::AssignmentOperator,
    pub left: Box<estree::Pattern>,
    pub right: Box<estree::Expression>,
}

impl Clone for AssignmentExpression {
    fn clone(&self) -> Self {
        Self {
            operator: self.operator,
            left: self.left.box_clone(),
            right: self.right.box_clone(),
        }
    }
}

impl estree::AssignmentExpression for AssignmentExpression {
    fn get_operator(&self) -> &estree::AssignmentOperator {
        &self.operator
    }

    fn get_left(&self) -> &Box<estree::Pattern> {
        &self.left
    }

    fn get_right(&self) -> &Box<estree::Expression> {
        &self.right
    }
}
impl estree::Expression for AssignmentExpression {
    fn box_clone(&self) -> Box<estree::Expression> {
        box (*self).clone()
    }
}
impl estree::Node for AssignmentExpression {}

#[derive(Debug)]
pub struct IfStatement {
    pub test: Box<estree::Expression>,
    pub consequent: Box<estree::Statement>,
    pub alternate: Option<Box<estree::Statement>>,
}

impl Clone for IfStatement {
    fn clone(&self) -> Self {
        Self {
            test: self.test.box_clone(),
            consequent: self.consequent.box_clone(),
            alternate: self.alternate.as_ref().map(|i| i.box_clone()),
        }
    }
}

impl estree::IfStatement for IfStatement {
    fn get_test(&self) -> &Box<estree::Expression> {
        &self.test
    }

    fn get_consequent(&self) -> &Box<estree::Statement> {
        &self.consequent
    }

    fn get_alternate(&self) -> &Option<Box<estree::Statement>> {
        &self.alternate
    }
}
impl estree::Statement for IfStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for IfStatement {}

#[derive(Debug)]
pub struct ExpressionStatement {
    pub expression: Box<estree::Expression>,
}

impl Clone for ExpressionStatement {
    fn clone(&self) -> Self {
        Self {
            expression: self.expression.box_clone(),
        }
    }
}

impl estree::ExpressionStatement for ExpressionStatement {
    fn get_expression(&self) -> &Box<estree::Expression> {
        &self.expression
    }
}
impl estree::Statement for ExpressionStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ExpressionStatement {}

#[derive(Debug)]
pub struct SequenceExpression {
    pub expressions: Vec<Box<estree::Expression>>,
}

impl Clone for SequenceExpression {
    fn clone(&self) -> Self {
        Self {
            expressions: self.expressions.iter().map(|i| i.box_clone()).collect(),
        }
    }
}

impl estree::SequenceExpression for SequenceExpression {
    fn get_expressions(&self) -> &Vec<Box<estree::Expression>> {
        &self.expressions
    }
}
impl estree::Expression for SequenceExpression {
    fn box_clone(&self) -> Box<estree::Expression> {
        box (*self).clone()
    }
}
impl estree::Node for SequenceExpression {}

#[derive(Debug)]
pub struct DoWhileStatement {
    pub body: Box<estree::Statement>,
    pub test: Box<estree::Expression>,
}

impl Clone for DoWhileStatement {
    fn clone(&self) -> Self {
        Self {
            body: self.body.box_clone(),
            test: self.test.box_clone(),
        }
    }
}

impl estree::DoWhileStatement for DoWhileStatement {
    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }

    fn get_test(&self) -> &Box<estree::Expression> {
        &self.test
    }
}
impl estree::Statement for DoWhileStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for DoWhileStatement {}

#[derive(Debug)]
pub struct WhileStatement {
    pub test: Box<estree::Expression>,
    pub body: Box<estree::Statement>,
}

impl Clone for WhileStatement {
    fn clone(&self) -> Self {
        Self {
            body: self.body.box_clone(),
            test: self.test.box_clone(),
        }
    }
}

impl estree::WhileStatement for WhileStatement {
    fn get_test(&self) -> &Box<estree::Expression> {
        &self.test
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}
impl estree::Statement for WhileStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for WhileStatement {}

#[derive(Debug)]
pub struct ForStatement {
    pub init: Option<estree::ForStatementInit>,
    pub test: Option<Box<estree::Expression>>,
    pub update: Option<Box<estree::Expression>>,
    pub body: Box<estree::Statement>,
}

impl Clone for ForStatement {
    fn clone(&self) -> Self {
        Self {
            init: self.init.as_ref().map(|i| i.clone()),
            test: self.test.as_ref().map(|i| i.box_clone()),
            update: self.update.as_ref().map(|i| i.box_clone()),
            body: self.body.box_clone(),
        }
    }
}

impl estree::ForStatement for ForStatement {
    fn get_init(&self) -> &Option<estree::ForStatementInit> {
        &self.init
    }

    fn get_test(&self) -> &Option<Box<estree::Expression>> {
        &self.test
    }

    fn get_update(&self) -> &Option<Box<estree::Expression>> {
        &self.update
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}
impl estree::Statement for ForStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ForStatement {}

#[derive(Debug)]
pub struct ForInStatement {
    pub left: estree::ForInStatementLeft,
    pub right: Box<estree::Expression>,
    pub body: Box<estree::Statement>,
}

impl Clone for ForInStatement {
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.box_clone(),
            body: self.body.box_clone(),
        }
    }
}

impl estree::ForInStatement for ForInStatement {
    fn get_left(&self) -> &estree::ForInStatementLeft {
        &self.left
    }

    fn get_right(&self) -> &Box<estree::Expression> {
        &self.right
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}

impl estree::Statement for ForInStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ForInStatement {}

#[derive(Debug)]
pub struct ForOfStatement {
    pub left: estree::ForInStatementLeft,
    pub right: Box<estree::Expression>,
    pub body: Box<estree::Statement>,
    pub await_: bool,
}

impl Clone for ForOfStatement {
    fn clone(&self) -> Self {
        Self {
            left: self.left.clone(),
            right: self.right.box_clone(),
            body: self.body.box_clone(),
            await_: self.await_,
        }
    }
}

impl estree::ForOfStatement for ForOfStatement {
    fn get_await(&self) -> bool {
        self.await_
    }
}

impl estree::ForInStatement for ForOfStatement {
    fn get_left(&self) -> &estree::ForInStatementLeft {
        &self.left
    }

    fn get_right(&self) -> &Box<estree::Expression> {
        &self.right
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}

impl estree::Statement for ForOfStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ForOfStatement {}

#[derive(Debug)]
pub struct SwitchStatement {
    pub discriminant: Box<estree::Expression>,
    pub case: Vec<Box<estree::SwitchCase>>,
}
impl Clone for SwitchStatement {
    fn clone(&self) -> Self {
        Self {
            discriminant: self.discriminant.box_clone(),
            case: self.case.iter().map(|i| i.box_clone()).collect(),
        }
    }
}

impl estree::SwitchStatement for SwitchStatement {
    fn get_discriminant(&self) -> &Box<estree::Expression> {
        &self.discriminant
    }

    fn get_cases(&self) -> &Vec<Box<estree::SwitchCase>> {
        &self.case
    }
}
impl estree::Statement for SwitchStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for SwitchStatement {}

#[derive(Debug)]
pub struct SwitchCase {
    pub test: Option<Box<estree::Expression>>,
    pub consequent: Vec<Box<estree::Statement>>,
}

impl Clone for SwitchCase {
    fn clone(&self) -> Self {
        Self {
            test: self.test.as_ref().map(|i| i.box_clone()),
            consequent: self.consequent.iter().map(|i| i.box_clone()).collect(),
        }
    }
}

impl estree::SwitchCase for SwitchCase {
    fn get_test(&self) -> &Option<Box<estree::Expression>> {
        &self.test
    }

    fn get_consequent(&self) -> &Vec<Box<estree::Statement>> {
        &self.consequent
    }

    fn box_clone(&self) -> Box<estree::SwitchCase> {
        box (*self).clone()
    }
}
impl estree::Node for SwitchCase {}

#[derive(Debug)]
pub struct ContinueStatement {
    pub label: Option<Box<estree::Identifier>>,
}

impl Clone for ContinueStatement {
    fn clone(&self) -> Self {
        Self {
            label: self
                .label
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
        }
    }
}

impl estree::ContinueStatement for ContinueStatement {
    fn get_label(&self) -> &Option<Box<estree::Identifier>> {
        &self.label
    }
}
impl estree::Statement for ContinueStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ContinueStatement {}

#[derive(Debug)]
pub struct BreakStatement {
    pub label: Option<Box<estree::Identifier>>,
}

impl Clone for BreakStatement {
    fn clone(&self) -> Self {
        Self {
            label: self
                .label
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
        }
    }
}

impl estree::BreakStatement for BreakStatement {
    fn get_label(&self) -> &Option<Box<estree::Identifier>> {
        &self.label
    }
}
impl estree::Statement for BreakStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for BreakStatement {}

#[derive(Debug)]
pub struct ReturnStatement {
    pub argument: Option<Box<estree::Expression>>,
}

impl Clone for ReturnStatement {
    fn clone(&self) -> Self {
        Self {
            argument: self.argument.as_ref().map(|i| i.box_clone()),
        }
    }
}

impl estree::ReturnStatement for ReturnStatement {
    fn get_argument(&self) -> &Option<Box<estree::Expression>> {
        &self.argument
    }
}
impl estree::Statement for ReturnStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ReturnStatement {}

#[derive(Debug)]
pub struct WithStatement {
    pub object: Box<estree::Expression>,
    pub body: Box<estree::Statement>,
}

impl Clone for WithStatement {
    fn clone(&self) -> Self {
        Self {
            object: self.object.box_clone(),
            body: self.body.box_clone(),
        }
    }
}

impl estree::WithStatement for WithStatement {
    fn get_object(&self) -> &Box<estree::Expression> {
        &self.object
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}
impl estree::Statement for WithStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for WithStatement {}

#[derive(Debug)]
pub struct LabeledStatement {
    pub label: Box<estree::Identifier>,
    pub body: Box<estree::Statement>,
}

impl Clone for LabeledStatement {
    fn clone(&self) -> Self {
        Self {
            label: estree::Identifier::box_clone(&*self.label),
            body: self.body.box_clone(),
        }
    }
}

impl estree::LabeledStatement for LabeledStatement {
    fn get_label(&self) -> &Box<estree::Identifier> {
        &self.label
    }

    fn get_body(&self) -> &Box<estree::Statement> {
        &self.body
    }
}
impl estree::Statement for LabeledStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for LabeledStatement {}

#[derive(Debug)]
pub struct ThrowStatement {
    pub argument: Box<estree::Expression>,
}

impl Clone for ThrowStatement {
    fn clone(&self) -> Self {
        Self {
            argument: self.argument.box_clone(),
        }
    }
}

impl estree::ThrowStatement for ThrowStatement {
    fn get_argument(&self) -> &Box<estree::Expression> {
        &self.argument
    }
}
impl estree::Statement for ThrowStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for ThrowStatement {}

#[derive(Debug)]
pub struct TryStatement {
    pub block: Box<estree::BlockStatement>,
    pub handler: Option<Box<estree::CatchClause>>,
    pub finalizer: Option<Box<estree::BlockStatement>>,
}
impl Clone for TryStatement {
    fn clone(&self) -> Self {
        Self {
            block: estree::BlockStatement::box_clone(&*self.block),
            handler: self
                .handler
                .as_ref()
                .map(|i| estree::CatchClause::box_clone(&**i)),
            finalizer: self
                .finalizer
                .as_ref()
                .map(|i| estree::BlockStatement::box_clone(&**i)),
        }
    }
}

impl estree::TryStatement for TryStatement {
    fn get_block(&self) -> &Box<estree::BlockStatement> {
        &self.block
    }

    fn get_handler(&self) -> &Option<Box<estree::CatchClause>> {
        &self.handler
    }

    fn get_finalizer(&self) -> &Option<Box<estree::BlockStatement>> {
        &self.finalizer
    }
}
impl estree::Statement for TryStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for TryStatement {}

#[derive(Debug)]
pub struct CatchClause {
    pub param: Box<estree::Pattern>,
    pub body: Box<estree::BlockStatement>,
}
impl Clone for CatchClause {
    fn clone(&self) -> Self {
        Self {
            param: self.param.box_clone(),
            body: estree::BlockStatement::box_clone(&*self.body),
        }
    }
}

impl estree::CatchClause for CatchClause {
    fn get_param(&self) -> &Box<estree::Pattern> {
        &self.param
    }

    fn get_body(&self) -> &Box<estree::BlockStatement> {
        &self.body
    }

    fn box_clone(&self) -> Box<estree::CatchClause> {
        box (*self).clone()
    }
}
impl estree::Node for CatchClause {}

#[derive(Debug, Clone)]
pub struct DebuggerStatement {}

impl estree::DebuggerStatement for DebuggerStatement {}
impl estree::Statement for DebuggerStatement {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for DebuggerStatement {}

#[derive(Debug)]
pub struct FunctionDeclaration {
    pub id: Option<Box<estree::Identifier>>,
    pub params: Vec<Box<estree::Pattern>>,
    pub body: Box<estree::FunctionBody>,
    pub generator: bool,
    pub async_: bool,
}

impl Clone for FunctionDeclaration {
    fn clone(&self) -> Self {
        Self {
            id: self
                .id
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
            params: self.params.iter().map(|i| i.box_clone()).collect(),
            body: estree::FunctionBody::box_clone(&*self.body),
            generator: self.generator,
            async_: self.async_,
        }
    }
}

impl estree::FunctionDeclaration for FunctionDeclaration {
    fn get_id(&self) -> &Box<estree::Identifier> {
        unimplemented!("should return correct id");
    }
}
impl estree::Function for FunctionDeclaration {
    fn get_id(&self) -> &Option<Box<estree::Identifier>> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Box<estree::Pattern>> {
        &self.params
    }

    fn get_body(&self) -> &Box<estree::FunctionBody> {
        &self.body
    }

    fn get_generator(&self) -> bool {
        self.generator
    }

    fn get_async(&self) -> bool {
        self.async_
    }
}
impl estree::Declaration for FunctionDeclaration {
    fn box_clone(&self) -> Box<estree::Declaration> {
        box (*self).clone()
    }
}

impl estree::Statement for FunctionDeclaration {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Node for FunctionDeclaration {}

#[derive(Debug)]
pub struct ClassDeclaration {
    pub id: Option<Box<estree::Identifier>>,
    pub super_class: Option<Box<estree::Expression>>,
    pub body: Box<estree::ClassBody>,
}

impl Clone for ClassDeclaration {
    fn clone(&self) -> Self {
        Self {
            id: self
                .id
                .as_ref()
                .map(|i| estree::Identifier::box_clone(&**i)),
            super_class: self
                .super_class
                .as_ref()
                .map(|i| estree::Expression::box_clone(&**i)),
            body: self.body.box_clone(),
        }
    }
}

impl estree::Class for ClassDeclaration {
    fn get_id(&self) -> &Option<Box<estree::Identifier>> {
        &self.id
    }

    fn get_super_class(&self) -> &Option<Box<estree::Expression>> {
        &self.super_class
    }

    fn get_body(&self) -> &Box<estree::ClassBody> {
        &self.body
    }
}

impl estree::ClassDeclaration for ClassDeclaration {
    fn get_id(&self) -> &Box<estree::Identifier> {
        unimplemented!();
    }
}
impl estree::Statement for ClassDeclaration {
    fn box_clone(&self) -> Box<estree::Statement> {
        box (*self).clone()
    }
}
impl estree::Declaration for ClassDeclaration {
    fn box_clone(&self) -> Box<estree::Declaration> {
        box (*self).clone()
    }
}
impl estree::Node for ClassDeclaration {}

#[derive(Debug)]
pub struct ClassBody {
    pub body: Vec<Box<estree::MethodDefinition>>,
}

impl Clone for ClassBody {
    fn clone(&self) -> Self {
        Self {
            body: self.body.iter().map(|p| p.box_clone()).collect(),
        }
    }
}

impl estree::ClassBody for ClassBody {
    fn get_body(&self) -> &Vec<Box<estree::MethodDefinition>> {
        &self.body
    }

    fn box_clone(&self) -> Box<estree::ClassBody> {
        box (*self).clone()
    }
}
impl estree::Node for ClassBody {}

#[derive(Debug)]
pub struct MethodDefinition {
    pub key: Box<estree::Expression>,
    pub value: Box<estree::FunctionExpression>,
    pub kind: estree::MethodDefinitionKind,
    pub computed: bool,
    pub static_: bool,
}

impl Clone for MethodDefinition {
    fn clone(&self) -> Self {
        Self {
            key: estree::Expression::box_clone(&*self.key),
            value: estree::FunctionExpression::box_clone(&*self.value),
            kind: self.kind,
            computed: self.computed,
            static_: self.static_,
        }
    }
}

impl estree::MethodDefinition for MethodDefinition {
    fn get_key(&self) -> &Box<estree::Expression> {
        &self.key
    }

    fn get_value(&self) -> &Box<estree::FunctionExpression> {
        &self.value
    }

    fn get_kind(&self) -> &estree::MethodDefinitionKind {
        &self.kind
    }

    fn get_computed(&self) -> bool {
        self.computed
    }

    fn get_static(&self) -> bool {
        self.static_
    }

    fn box_clone(&self) -> Box<estree::MethodDefinition> {
        box (*self).clone()
    }
}
impl estree::Node for MethodDefinition {}
