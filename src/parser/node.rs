use crate::{lexer::token::Number, parser::estree};
use std::convert::TryFrom;

#[derive(Debug, Clone)]
pub enum Expression {
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral(NullLiteral),
    NumberLiteral(NumberLiteral),
    Regex(Regex),
    Identifier(Identifier),
    ObjectExpression(ObjectExpression),
    ArrowFunctionExpression(ArrowFunctionExpression),
    FunctionExpression(FunctionExpression),
    AssignmentExpression(AssignmentExpression),
    ExpressionStatement(ExpressionStatement),
    SequenceExpression(SequenceExpression),
    ThisExpression(ThisExpression),
    ArrayExpression(ArrayExpression),
    LogicalExpression(LogicalExpression),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    UpdateExpression(UpdateExpression),
    NewExpression(NewExpression),
}

impl estree::Expression for Expression {}
impl estree::Node for Expression {}

#[derive(Debug, Clone)]
pub enum Node {
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral(NullLiteral),
    NumberLiteral(NumberLiteral),
    Regex(Regex),
    VariableDeclarator(VariableDeclarator),
    ObjectPattern(ObjectPattern),
    Identifier(Identifier),
    ObjectExpression(ObjectExpression),
    RestElement(RestElement),
    AssignmentProperty(AssignmentProperty),
    AssignmentPattern(AssignmentPattern),
    BlockStatement(BlockStatement),
    EmptyStatement(EmptyStatement),
    ArrowFunctionExpression(ArrowFunctionExpression),
    FunctionExpression(FunctionExpression),
    FunctionBody(FunctionBody),
    AssignmentExpression(AssignmentExpression),
    IfStatement(IfStatement),
    ExpressionStatement(ExpressionStatement),
    SequenceExpression(SequenceExpression),
    DoWhileStatement(DoWhileStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    ForInStatement(ForInStatement),
    ForOfStatement(ForOfStatement),
    SwitchStatement(SwitchStatement),
    SwitchCase(SwitchCase),
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    ReturnStatement(ReturnStatement),
    WithStatement(WithStatement),
    LabeledStatement(LabeledStatement),
    ThrowStatement(ThrowStatement),
    TryStatement(TryStatement),
    CatchClause(CatchClause),
    DebuggerStatement(DebuggerStatement),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    ClassBody(ClassBody),
    MethodDefinition(MethodDefinition),
    ThisExpression(ThisExpression),
    ArrayExpression(ArrayExpression),
    SpreadElement(SpreadElement),
    LogicalExpression(LogicalExpression),
    BinaryExpression(BinaryExpression),
    UnaryExpression(UnaryExpression),
    UpdateExpression(UpdateExpression),
    NewExpression(NewExpression),
}

impl estree::Node for Node {}

#[derive(Debug, Clone)]
pub enum Literal {
    StringLiteral(StringLiteral),
    BooleanLiteral(BooleanLiteral),
    NullLiteral(NullLiteral),
    NumberLiteral(NumberLiteral),
    Regex(Regex),
}

#[derive(Debug, Clone)]
pub enum Declaration {
    VariableDeclaration(VariableDeclaration),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
}

impl From<Declaration> for Statement {
    fn from(decl: Declaration) -> Self {
        match decl {
            Declaration::VariableDeclaration(var) => Statement::VariableDeclaration(var),
            Declaration::FunctionDeclaration(fun) => Statement::FunctionDeclaration(fun),
            Declaration::ClassDeclaration(class) => Statement::ClassDeclaration(class),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Statement {
    VariableDeclaration(VariableDeclaration),
    BlockStatement(BlockStatement),
    EmptyStatement(EmptyStatement),
    FunctionBody(FunctionBody),
    IfStatement(IfStatement),
    ExpressionStatement(ExpressionStatement),
    DoWhileStatement(DoWhileStatement),
    WhileStatement(WhileStatement),
    ForStatement(ForStatement),
    ForInStatement(ForInStatement),
    ForOfStatement(ForOfStatement),
    SwitchStatement(SwitchStatement),
    ContinueStatement(ContinueStatement),
    BreakStatement(BreakStatement),
    ReturnStatement(ReturnStatement),
    WithStatement(WithStatement),
    LabeledStatement(LabeledStatement),
    ThrowStatement(ThrowStatement),
    TryStatement(TryStatement),
    DebuggerStatement(DebuggerStatement),
    FunctionDeclaration(FunctionDeclaration),
    ClassDeclaration(ClassDeclaration),
    Property(property),
}

impl estree::Statement for Statement {}

impl estree::Node for Statement {}

#[derive(Debug, Clone)]
pub enum Pattern {
    ObjectPattern(ObjectPattern),
    ArrayPattern(ArrayPattern),
    Identifier(Identifier),
    RestElement(RestElement),
    AssignmentPattern(AssignmentPattern),
}

impl From<Expression> for Pattern {
    fn from(pat: Expression) -> Self {
        match pat {
            Expression::Identifier(pat) => Pattern::Identifier(pat),
            _ => unreachable!(),
        }
    }
}

impl estree::Pattern for Pattern {}

impl estree::Node for Pattern {}

#[derive(Debug, Clone)]
pub enum Property {
    AssignmentProperty(AssignmentProperty),
    Property(property),
}

#[derive(Debug, Clone)]
pub enum Class {
    ClassDeclaration(ClassDeclaration),
}

#[derive(Debug, Clone)]
pub enum Function {
    ArrowFunctionExpression(ArrowFunctionExpression),
    FunctionExpression(FunctionExpression),
    FunctionDeclaration(FunctionDeclaration),
}

#[derive(Debug, Clone)]
pub struct StringLiteral(pub String);
impl estree::Literal for StringLiteral {}
impl estree::Node for StringLiteral {}
impl estree::Expression for StringLiteral {}

#[derive(Debug, Clone)]
pub struct BooleanLiteral(pub bool);
impl estree::Literal for BooleanLiteral {}
impl estree::Node for BooleanLiteral {}
impl estree::Expression for BooleanLiteral {}

#[derive(Debug, Clone)]
pub struct NullLiteral;
impl estree::Literal for NullLiteral {}
impl estree::Node for NullLiteral {}
impl estree::Expression for NullLiteral {}

#[derive(Debug, Clone)]
pub struct NumberLiteral(pub Number);
impl estree::Literal for NumberLiteral {}
impl estree::Node for NumberLiteral {}
impl estree::Expression for NumberLiteral {}

#[derive(Debug, Clone)]
pub struct Regex {}
impl estree::Literal for Regex {}
impl estree::Node for Regex {}
impl estree::Expression for Regex {}

#[derive(Debug, Clone)]
pub struct Program {
    pub sourceType: estree::ProgramSourceType,
    pub body: Vec<ProgramBody>,
}

impl estree::Program for Program {
    fn get_body(&self) -> &Vec<ProgramBody> {
        &self.body
    }

    fn get_source_type(&self) -> &estree::ProgramSourceType {
        &self.sourceType
    }
}

impl estree::Node for Program {}

#[derive(Debug, Clone)]
pub enum ProgramBody {
    ProgramDirective,
    ProgramStatement(Statement),
}

#[derive(Debug, Clone)]
pub struct VariableDeclarator {
    pub id: Pattern,
    pub init: Option<Expression>,
}

impl estree::VariableDeclarator for VariableDeclarator {
    fn get_id(&self) -> &estree::Pattern {
        &self.id
    }

    fn get_init(&self) -> &Option<Expression> {
        &self.init
    }
}
impl estree::Node for VariableDeclarator {}

#[derive(Debug, Clone)]
pub struct VariableDeclaration {
    pub kind: estree::VariableDeclarationKind,
    pub declarations: Vec<VariableDeclarator>,
}

impl estree::VariableDeclaration for VariableDeclaration {
    fn get_declarations(&self) -> &Vec<VariableDeclarator> {
        &self.declarations
    }

    fn get_kind(&self) -> &estree::VariableDeclarationKind {
        &self.kind
    }
}

impl estree::Declaration for VariableDeclaration {}

impl estree::Statement for VariableDeclaration {}
impl estree::Node for VariableDeclaration {}

#[derive(Debug, Clone)]
pub enum ObjectPatternProperty {
    AssignmentProperty(AssignmentProperty),
    RestElement(RestElement),
}

#[derive(Debug, Clone)]
pub struct ObjectPattern {
    pub properties: Vec<ObjectPatternProperty>,
}

impl estree::ObjectPattern for ObjectPattern {
    fn get_properties(&self) -> &Vec<ObjectPatternProperty> {
        &self.properties
    }
}
impl estree::Pattern for ObjectPattern {}
impl estree::Node for ObjectPattern {}

#[derive(Debug, Clone)]
pub struct ArrayPattern {
    pub elements: Vec<Option<Pattern>>,
}

impl estree::ArrayPattern for ArrayPattern {
    fn get_elements(&self) -> &Vec<Option<Pattern>> {
        &self.elements
    }
}
impl estree::Pattern for ArrayPattern {}
impl estree::Node for ArrayPattern {}

#[derive(Clone, Debug)]
pub struct Identifier {
    pub name: String,
}

impl estree::Identifier for Identifier {
    fn get_name(&self) -> &str {
        self.name.as_str()
    }
}
impl estree::Pattern for Identifier {}
impl estree::Node for Identifier {}
impl estree::Expression for Identifier {}

impl From<String> for Identifier {
    fn from(name: String) -> Self {
        Identifier { name }
    }
}

#[derive(Debug, Clone)]
pub enum ObjectExpressionProperty {
    Property(property),
    SpreadElement(SpreadElement),
}

#[derive(Debug, Clone)]
pub struct ObjectExpression {
    pub properties: Vec<ObjectExpressionProperty>,
}

impl estree::ObjectExpression for ObjectExpression {
    fn get_properties(&self) -> &Vec<ObjectExpressionProperty> {
        &self.properties
    }
}
impl estree::Node for ObjectExpression {}
impl estree::Expression for ObjectExpression {}

#[derive(Debug, Clone)]
pub struct RestElement {
    pub argument: Box<Pattern>,
}

impl estree::RestElement for RestElement {
    fn get_argument(&self) -> &estree::Pattern {
        &*self.argument
    }
}
impl estree::Pattern for RestElement {}
impl estree::Node for RestElement {}

#[derive(Debug, Clone)]
pub struct AssignmentProperty {
    pub key: Expression,
    pub value: Pattern,
    pub shorthand: bool,
}

impl estree::AssignmentProperty for AssignmentProperty {
    fn get_value(&self) -> &estree::Pattern {
        &self.value
    }
}

impl estree::Property for AssignmentProperty {
    fn get_key(&self) -> &estree::Expression {
        &self.key
    }

    fn get_value(&self) -> &estree::Expression {
        unreachable!();
    }

    fn get_kind(&self) -> &estree::PropertyKind {
        <Self as estree::AssignmentProperty>::get_kind(self)
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
}
impl estree::Node for AssignmentProperty {}

#[derive(Debug, Clone)]
pub struct AssignmentPattern {
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

impl estree::AssignmentPattern for AssignmentPattern {
    fn get_left(&self) -> &estree::Pattern {
        &*self.left
    }

    fn get_right(&self) -> &estree::Expression {
        &*self.right
    }
}
impl estree::Pattern for AssignmentPattern {}
impl estree::Node for AssignmentPattern {}

#[derive(Debug, Clone)]
pub struct BlockStatement {
    pub body: Vec<Statement>,
}

impl estree::BlockStatement for BlockStatement {
    fn get_body(&self) -> &Vec<Statement> {
        &self.body
    }
}
impl estree::Statement for BlockStatement {}
impl estree::Node for BlockStatement {}

#[derive(Debug, Clone)]
pub struct EmptyStatement {}

impl estree::EmptyStatement for EmptyStatement {}
impl estree::Statement for EmptyStatement {}
impl estree::Node for EmptyStatement {}

#[derive(Debug, Clone)]
pub enum ArrowFunctionExpressionBody {
    FunctionBody(FunctionBody),
    Expression(Box<Expression>),
}

#[derive(Debug, Clone)]
pub struct ArrowFunctionExpression {
    pub body: ArrowFunctionExpressionBody,
    pub expression: bool,
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub generator: bool,
    pub async_: bool,
}

impl estree::ArrowFunctionExpression for ArrowFunctionExpression {
    fn get_body(&self) -> &ArrowFunctionExpressionBody {
        &self.body
    }

    fn get_expression(&self) -> bool {
        self.expression
    }
}
impl estree::Expression for ArrowFunctionExpression {}
impl estree::Function for ArrowFunctionExpression {
    fn get_id(&self) -> &Option<Identifier> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Pattern> {
        &self.params
    }

    fn get_body(&self) -> &estree::FunctionBody {
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

#[derive(Debug, Clone)]
pub struct FunctionExpression {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: FunctionBody,
    pub generator: bool,
    pub async_: bool,
}

impl estree::FunctionExpression for FunctionExpression {}
impl estree::Function for FunctionExpression {
    fn get_id(&self) -> &Option<Identifier> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Pattern> {
        &self.params
    }

    fn get_body(&self) -> &estree::FunctionBody {
        &self.body
    }

    fn get_generator(&self) -> bool {
        self.generator
    }

    fn get_async(&self) -> bool {
        self.async_
    }
}
impl estree::Expression for FunctionExpression {}
impl estree::Node for FunctionExpression {}

#[derive(Debug, Clone)]
pub enum FunctionBodyEnum {
    // Directive(Directive),
    Statement(Statement),
}

#[derive(Debug, Clone)]
pub struct FunctionBody {
    pub body: Vec<FunctionBodyEnum>,
}

impl estree::FunctionBody for FunctionBody {
    fn get_body(&self) -> &Vec<FunctionBodyEnum> {
        &self.body
    }
}
impl estree::BlockStatement for FunctionBody {
    fn get_body(&self) -> &Vec<Statement> {
        unimplemented!()
    }
}
impl estree::Statement for FunctionBody {}
impl estree::Node for FunctionBody {}

#[derive(Debug, Clone)]
pub struct AssignmentExpression {
    pub operator: estree::AssignmentOperator,
    pub left: Box<Pattern>,
    pub right: Box<Expression>,
}

impl estree::AssignmentExpression for AssignmentExpression {
    fn get_operator(&self) -> &estree::AssignmentOperator {
        &self.operator
    }

    fn get_left(&self) -> &estree::Pattern {
        &*self.left
    }

    fn get_right(&self) -> &estree::Expression {
        &*self.right
    }
}
impl estree::Expression for AssignmentExpression {}
impl estree::Node for AssignmentExpression {}

#[derive(Debug, Clone)]
pub struct IfStatement {
    pub test: Expression,
    pub consequent: Box<Statement>,
    pub alternate: Option<Box<Statement>>,
}

impl estree::IfStatement for IfStatement {
    fn get_test(&self) -> &estree::Expression {
        &self.test
    }

    fn get_consequent(&self) -> &estree::Statement {
        &*self.consequent
    }

    fn get_alternate(&self) -> &Option<Box<Statement>> {
        &self.alternate
    }
}
impl estree::Statement for IfStatement {}
impl estree::Node for IfStatement {}

#[derive(Debug, Clone)]
pub struct ExpressionStatement {
    pub expression: Box<Expression>,
}

impl estree::ExpressionStatement for ExpressionStatement {
    fn get_expression(&self) -> &estree::Expression {
        &*self.expression
    }
}
impl estree::Statement for ExpressionStatement {}
impl estree::Node for ExpressionStatement {}

#[derive(Debug, Clone)]
pub struct SequenceExpression {
    pub expressions: Vec<Expression>,
}

impl estree::SequenceExpression for SequenceExpression {
    fn get_expressions(&self) -> &Vec<Expression> {
        &self.expressions
    }
}
impl estree::Expression for SequenceExpression {}
impl estree::Node for SequenceExpression {}

#[derive(Debug, Clone)]
pub struct DoWhileStatement {
    pub body: Box<Statement>,
    pub test: Box<Expression>,
}

impl estree::DoWhileStatement for DoWhileStatement {
    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }

    fn get_test(&self) -> &estree::Expression {
        &*self.test
    }
}
impl estree::Statement for DoWhileStatement {}
impl estree::Node for DoWhileStatement {}

#[derive(Debug, Clone)]
pub struct WhileStatement {
    pub test: Expression,
    pub body: Box<Statement>,
}

impl estree::WhileStatement for WhileStatement {
    fn get_test(&self) -> &estree::Expression {
        &self.test
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}
impl estree::Statement for WhileStatement {}
impl estree::Node for WhileStatement {}

#[derive(Debug, Clone)]
pub enum ForStatementInit {
    VariableDeclaration(VariableDeclaration),
    Expression(Expression),
}

#[derive(Debug, Clone)]
pub struct ForStatement {
    pub init: Option<ForStatementInit>,
    pub test: Option<Expression>,
    pub update: Option<Expression>,
    pub body: Box<Statement>,
}

impl estree::ForStatement for ForStatement {
    fn get_init(&self) -> &Option<ForStatementInit> {
        &self.init
    }

    fn get_test(&self) -> &Option<Expression> {
        &self.test
    }

    fn get_update(&self) -> &Option<Expression> {
        &self.update
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}
impl estree::Statement for ForStatement {}
impl estree::Node for ForStatement {}

#[derive(Debug, Clone)]
pub enum ForInStatementLeft {
    VariableDeclaration(VariableDeclaration),
    Pattern(Pattern),
}

#[derive(Debug, Clone)]
pub struct ForInStatement {
    pub left: ForInStatementLeft,
    pub right: Expression,
    pub body: Box<Statement>,
}

impl estree::ForInStatement for ForInStatement {
    fn get_left(&self) -> &ForInStatementLeft {
        &self.left
    }

    fn get_right(&self) -> &estree::Expression {
        &self.right
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}

impl estree::Statement for ForInStatement {}
impl estree::Node for ForInStatement {}

#[derive(Debug, Clone)]
pub struct ForOfStatement {
    pub left: ForInStatementLeft,
    pub right: Expression,
    pub body: Box<Statement>,
    pub await_: bool,
}

impl estree::ForOfStatement for ForOfStatement {
    fn get_await(&self) -> bool {
        self.await_
    }
}

impl estree::ForInStatement for ForOfStatement {
    fn get_left(&self) -> &ForInStatementLeft {
        &self.left
    }

    fn get_right(&self) -> &estree::Expression {
        &self.right
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}

impl estree::Statement for ForOfStatement {}
impl estree::Node for ForOfStatement {}

#[derive(Debug, Clone)]
pub struct SwitchStatement {
    pub discriminant: Expression,
    pub case: Vec<SwitchCase>,
}

impl estree::SwitchStatement for SwitchStatement {
    fn get_discriminant(&self) -> &estree::Expression {
        &self.discriminant
    }

    fn get_cases(&self) -> &Vec<SwitchCase> {
        &self.case
    }
}
impl estree::Statement for SwitchStatement {}
impl estree::Node for SwitchStatement {}

#[derive(Debug, Clone)]
pub struct SwitchCase {
    pub test: Option<Expression>,
    pub consequent: Vec<Statement>,
}

impl estree::SwitchCase for SwitchCase {
    fn get_test(&self) -> &Option<Expression> {
        &self.test
    }

    fn get_consequent(&self) -> &Vec<Statement> {
        &self.consequent
    }
}
impl estree::Node for SwitchCase {}

#[derive(Debug, Clone)]
pub struct ContinueStatement {
    pub label: Option<Identifier>,
}

impl estree::ContinueStatement for ContinueStatement {
    fn get_label(&self) -> &Option<Identifier> {
        &self.label
    }
}
impl estree::Statement for ContinueStatement {}
impl estree::Node for ContinueStatement {}

#[derive(Debug, Clone)]
pub struct BreakStatement {
    pub label: Option<Identifier>,
}

impl estree::BreakStatement for BreakStatement {
    fn get_label(&self) -> &Option<Identifier> {
        &self.label
    }
}
impl estree::Statement for BreakStatement {}
impl estree::Node for BreakStatement {}

#[derive(Debug, Clone)]
pub struct ReturnStatement {
    pub argument: Option<Expression>,
}

impl estree::ReturnStatement for ReturnStatement {
    fn get_argument(&self) -> &Option<Expression> {
        &self.argument
    }
}
impl estree::Statement for ReturnStatement {}
impl estree::Node for ReturnStatement {}

#[derive(Debug, Clone)]
pub struct WithStatement {
    pub object: Expression,
    pub body: Box<Statement>,
}

impl estree::WithStatement for WithStatement {
    fn get_object(&self) -> &estree::Expression {
        &self.object
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}
impl estree::Statement for WithStatement {}
impl estree::Node for WithStatement {}

#[derive(Debug, Clone)]
pub struct LabeledStatement {
    pub label: Identifier,
    pub body: Box<Statement>,
}

impl estree::LabeledStatement for LabeledStatement {
    fn get_label(&self) -> &estree::Identifier {
        &self.label
    }

    fn get_body(&self) -> &estree::Statement {
        &*self.body
    }
}
impl estree::Statement for LabeledStatement {}
impl estree::Node for LabeledStatement {}

#[derive(Debug, Clone)]
pub struct ThrowStatement {
    pub argument: Expression,
}

impl estree::ThrowStatement for ThrowStatement {
    fn get_argument(&self) -> &estree::Expression {
        &self.argument
    }
}
impl estree::Statement for ThrowStatement {}
impl estree::Node for ThrowStatement {}

#[derive(Debug, Clone)]
pub struct TryStatement {
    pub block: BlockStatement,
    pub handler: Option<CatchClause>,
    pub finalizer: Option<BlockStatement>,
}

impl estree::TryStatement for TryStatement {
    fn get_block(&self) -> &estree::BlockStatement {
        &self.block
    }

    fn get_handler(&self) -> &Option<CatchClause> {
        &self.handler
    }

    fn get_finalizer(&self) -> &Option<BlockStatement> {
        &self.finalizer
    }
}
impl estree::Statement for TryStatement {}
impl estree::Node for TryStatement {}

#[derive(Debug, Clone)]
pub struct CatchClause {
    pub param: Pattern,
    pub body: BlockStatement,
}

impl estree::CatchClause for CatchClause {
    fn get_param(&self) -> &estree::Pattern {
        &self.param
    }

    fn get_body(&self) -> &estree::BlockStatement {
        &self.body
    }
}
impl estree::Node for CatchClause {}

#[derive(Debug, Clone)]
pub struct DebuggerStatement {}

impl estree::DebuggerStatement for DebuggerStatement {}
impl estree::Statement for DebuggerStatement {}
impl estree::Node for DebuggerStatement {}

#[derive(Debug, Clone)]
pub struct FunctionDeclaration {
    pub id: Option<Identifier>,
    pub params: Vec<Pattern>,
    pub body: FunctionBody,
    pub generator: bool,
    pub async_: bool,
}

impl estree::FunctionDeclaration for FunctionDeclaration {
    fn get_id(&self) -> &estree::Identifier {
        unimplemented!("should return correct id");
    }
}
impl estree::Function for FunctionDeclaration {
    fn get_id(&self) -> &Option<Identifier> {
        &self.id
    }

    fn get_params(&self) -> &Vec<Pattern> {
        &self.params
    }

    fn get_body(&self) -> &estree::FunctionBody {
        &self.body
    }

    fn get_generator(&self) -> bool {
        self.generator
    }

    fn get_async(&self) -> bool {
        self.async_
    }
}
impl estree::Declaration for FunctionDeclaration {}

impl estree::Statement for FunctionDeclaration {}
impl estree::Node for FunctionDeclaration {}

#[derive(Debug, Clone)]
pub struct ClassDeclaration {
    pub id: Option<Identifier>,
    pub super_class: Option<Expression>,
    pub body: ClassBody,
}

impl estree::Class for ClassDeclaration {
    fn get_id(&self) -> &Option<Identifier> {
        &self.id
    }

    fn get_super_class(&self) -> &Option<Expression> {
        &self.super_class
    }

    fn get_body(&self) -> &estree::ClassBody {
        &self.body
    }
}

impl estree::ClassDeclaration for ClassDeclaration {
    fn get_id(&self) -> &estree::Identifier {
        unimplemented!();
    }
}
impl estree::Statement for ClassDeclaration {}
impl estree::Declaration for ClassDeclaration {}
impl estree::Node for ClassDeclaration {}

#[derive(Debug, Clone)]
pub struct ClassBody {
    pub body: Vec<MethodDefinition>,
}

impl estree::ClassBody for ClassBody {
    fn get_body(&self) -> &Vec<MethodDefinition> {
        &self.body
    }
}
impl estree::Node for ClassBody {}

#[derive(Debug, Clone)]
pub struct MethodDefinition {
    pub key: Expression,
    pub value: FunctionExpression,
    pub kind: estree::MethodDefinitionKind,
    pub computed: bool,
    pub static_: bool,
}

impl estree::MethodDefinition for MethodDefinition {
    fn get_key(&self) -> &estree::Expression {
        &self.key
    }

    fn get_value(&self) -> &estree::FunctionExpression {
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
}
impl estree::Node for MethodDefinition {}

#[derive(Debug, Clone)]
pub struct ThisExpression {}
impl estree::ThisExpression for ThisExpression {}
impl estree::Expression for ThisExpression {}
impl estree::Node for ThisExpression {}

#[derive(Debug, Clone)]
pub enum ArrayExpressionElement {
    Expression(Expression),
    SpreadElement(SpreadElement),
}

#[derive(Debug, Clone)]
pub struct ArrayExpression {
    pub elements: Vec<Option<ArrayExpressionElement>>,
}
impl estree::ArrayExpression for ArrayExpression {
    fn get_elements(&self) -> &Vec<Option<ArrayExpressionElement>> {
        &self.elements
    }
}
impl estree::Expression for ArrayExpression {}
impl estree::Node for ArrayExpression {}

#[derive(Debug, Clone)]
pub struct SpreadElement {
    pub argument: Expression,
}

impl estree::SpreadElement for SpreadElement {
    fn get_argument(&self) -> &estree::Expression {
        &self.argument
    }
}
impl estree::Node for SpreadElement {}

#[derive(Debug, Clone)]
pub struct property {
    pub key: Expression,
    pub value: Expression,
    pub kind: estree::PropertyKind,
    pub method: bool,
    pub shorthand: bool,
    pub computed: bool,
}

impl estree::Property for property {
    fn get_key(&self) -> &estree::Expression {
        &self.key
    }

    fn get_value(&self) -> &estree::Expression {
        &self.value
    }

    fn get_kind(&self) -> &estree::PropertyKind {
        &self.kind
    }

    fn get_method(&self) -> bool {
        self.method
    }

    fn get_shorthand(&self) -> bool {
        self.shorthand
    }

    fn get_computed(&self) -> bool {
        self.computed
    }
}
impl estree::Node for property {}

#[derive(Debug, Clone)]
pub struct LogicalExpression {
    pub operator: estree::LogicalOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}
impl estree::LogicalExpression for LogicalExpression {
    fn get_operator(&self) -> estree::LogicalOperator {
        self.operator
    }

    fn get_left(&self) -> &estree::Expression {
        &*self.left
    }

    fn get_right(&self) -> &estree::Expression {
        &*self.right
    }
}
impl estree::Node for LogicalExpression {}
impl estree::Expression for LogicalExpression {}

#[derive(Debug, Clone)]
pub struct BinaryExpression {
    pub operator: estree::BinaryOperator,
    pub left: Box<Expression>,
    pub right: Box<Expression>,
}

impl estree::BinaryExpression for BinaryExpression {
    fn get_operator(&self) -> &estree::BinaryOperator {
        &self.operator
    }

    fn get_left(&self) -> &Expression {
        &self.left
    }

    fn get_right(&self) -> &Expression {
        &self.right
    }
}
impl estree::Node for BinaryExpression {}
impl estree::Expression for BinaryExpression {}

#[derive(Debug, Clone)]
pub struct UnaryExpression {
    pub operator: estree::UnaryOperator,
    pub prefix: bool,
    pub argument: Box<Expression>,
}
impl estree::UnaryExpression for UnaryExpression {
    fn get_operator(&self) -> &estree::UnaryOperator {
        &self.operator
    }

    fn get_prefix(&self) -> bool {
        self.prefix
    }

    fn get_argument(&self) -> &Expression {
        &self.argument
    }
}
impl estree::Expression for UnaryExpression {}
impl estree::Node for UnaryExpression {}

#[derive(Debug, Clone)]
pub struct UpdateExpression {
    pub operator: estree::UpdateOperator,
    pub argument: Box<Expression>,
    pub prefix: bool,
}

impl estree::UpdateExpression for UpdateExpression {
    fn get_operator(&self) -> &estree::UpdateOperator {
        &self.operator
    }

    fn get_argument(&self) -> &Expression {
        &self.argument
    }

    fn get_prefix(&self) -> bool {
        self.prefix
    }
}
impl estree::Expression for UpdateExpression {}
impl estree::Node for UpdateExpression {}

#[derive(Clone, Debug)]
pub struct NewExpression {
    pub callee: Box<Expression>,
    pub arguments: Vec<estree::NewExpressionArgument>,
}
impl estree::NewExpression for NewExpression {
    fn get_callee(&self) -> &Expression {
        &self.callee
    }

    fn get_arguments(&self) -> &Vec<estree::NewExpressionArgument> {
        &self.arguments
    }
}
impl estree::Expression for NewExpression {}
impl estree::Node for NewExpression {}

#[derive(Debug, Clone)]
pub struct Super {}
impl estree::Super for Super {}
impl estree::Node for Super {}
