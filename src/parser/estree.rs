use crate::{lexer, parser::node};
use mopa;
/// AST tree interfaces as specified in ESTree standart
/// https://github.com/estree/estree/
use std::fmt::Debug;

pub trait Node: Debug + mopa::Any {}
mopafy!(Node);

struct SourceLocation {
    source: Option<String>,
    start: Position,
    end: Position,
}

struct Position {
    line: i32,   // >= 1
    column: i32, // >= 0
}

pub trait Identifier: Expression + Pattern {
    fn get_name(&self) -> &str;
}

pub trait Literal: Expression {}

struct Regex {}

trait RegExpLiteral: Literal {
    fn get_regex(&self) -> &Regex;
}
pub trait Program: Node {
    fn get_body(&self) -> &Vec<node::ProgramBody>;
    fn get_source_type(&self) -> &ProgramSourceType;
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum ProgramSourceType {
    Script,
    Module,
}

#[derive(Debug)]
pub enum ProgramStatement {
    VariableDeclaration(Box<VariableDeclaration>),
    BlockStatement(Box<BlockStatement>),
}

pub trait Function: Node {
    fn get_id(&self) -> &Option<node::Identifier>;
    fn get_params(&self) -> &Vec<node::Pattern>;
    fn get_body(&self) -> &FunctionBody;
    fn get_generator(&self) -> bool;
    fn get_async(&self) -> bool;
}

pub trait Statement: Node {}
mopafy!(Statement);

pub trait ExpressionStatement: Statement {
    fn get_expression(&self) -> &Expression;
}

pub trait Directive: Node {
    fn get_expression(&self) -> &Literal;
    fn get_directive(&self) -> &String;
}

pub trait BlockStatement: Statement {
    fn get_body(&self) -> &Vec<node::Statement>;
}

pub trait FunctionBody: BlockStatement {
    fn get_body(&self) -> &Vec<node::FunctionBodyEnum>;
}

pub trait EmptyStatement: Statement {}

pub trait DebuggerStatement: Statement {}

pub trait WithStatement: Statement {
    fn get_object(&self) -> &Expression;
    fn get_body(&self) -> &Statement;
}

pub trait ReturnStatement: Statement {
    fn get_argument(&self) -> &Option<node::Expression>;
}

pub trait LabeledStatement: Statement {
    fn get_label(&self) -> &Identifier;
    fn get_body(&self) -> &Statement;
}

pub trait BreakStatement: Statement {
    fn get_label(&self) -> &Option<node::Identifier>;
}

pub trait ContinueStatement: Statement {
    fn get_label(&self) -> &Option<node::Identifier>;
}

pub trait IfStatement: Statement {
    fn get_test(&self) -> &Expression;
    fn get_consequent(&self) -> &Statement;
    fn get_alternate(&self) -> &Option<Box<node::Statement>>;
}

pub trait SwitchStatement: Statement {
    fn get_discriminant(&self) -> &Expression;
    fn get_cases(&self) -> &Vec<node::SwitchCase>;
}

pub trait SwitchCase: Node {
    fn get_test(&self) -> &Option<node::Expression>;
    fn get_consequent(&self) -> &Vec<node::Statement>;
}

pub trait ThrowStatement: Statement {
    fn get_argument(&self) -> &Expression;
}

pub trait TryStatement: Statement {
    fn get_block(&self) -> &BlockStatement;
    fn get_handler(&self) -> &Option<node::CatchClause>;
    fn get_finalizer(&self) -> &Option<node::BlockStatement>;
}

pub trait CatchClause: Node {
    fn get_param(&self) -> &Pattern;
    fn get_body(&self) -> &BlockStatement;
}

pub trait WhileStatement: Statement {
    fn get_test(&self) -> &Expression;
    fn get_body(&self) -> &Statement;
}

pub trait DoWhileStatement: Statement {
    fn get_body(&self) -> &Statement;
    fn get_test(&self) -> &Expression;
}

pub trait ForStatement: Statement {
    fn get_init(&self) -> &Option<node::ForStatementInit>;
    fn get_test(&self) -> &Option<node::Expression>;
    fn get_update(&self) -> &Option<node::Expression>;
    fn get_body(&self) -> &Statement;
}

pub trait ForInStatement: Statement {
    fn get_left(&self) -> &node::ForInStatementLeft;
    fn get_right(&self) -> &Expression;
    fn get_body(&self) -> &Statement;
}

pub trait ForOfStatement: ForInStatement {
    fn get_await(&self) -> bool;
}

pub trait Declaration: Statement {}

// Declarations
pub trait FunctionDeclaration: Function + Declaration {
    fn get_id(&self) -> &Identifier;
}

pub trait VariableDeclaration: Declaration {
    fn get_declarations(&self) -> &Vec<node::VariableDeclarator>;
    fn get_kind(&self) -> &VariableDeclarationKind;
}

#[derive(Debug, PartialEq, Clone)]
pub enum VariableDeclarationKind {
    Const,
    Let,
    Var,
}

impl From<lexer::token::Token> for VariableDeclarationKind {
    fn from(other: lexer::token::Token) -> Self {
        use crate::lexer::token::Token;
        match other {
            Token::KLet => VariableDeclarationKind::Let,
            Token::KVar => VariableDeclarationKind::Var,
            Token::KConst => VariableDeclarationKind::Const,
            _ => unreachable!(),
        }
    }
}

pub trait VariableDeclarator: Node {
    fn get_id(&self) -> &Pattern;
    fn get_init(&self) -> &Option<node::Expression>;
}

pub trait Expression: Node {}
mopafy!(Expression);

pub trait ThisExpression: Expression {}

pub trait ArrayExpression: Expression {
    fn get_elements(&self) -> &Vec<Option<node::ArrayExpressionElement>>;
}

pub trait ObjectExpression: Expression {
    fn get_properties(&self) -> &Vec<node::ObjectExpressionProperty>;
}

pub trait Property: Node {
    fn get_key(&self) -> &Expression;
    fn get_value(&self) -> &Expression;
    fn get_kind(&self) -> &PropertyKind;
    fn get_method(&self) -> bool;
    fn get_shorthand(&self) -> bool;
    fn get_computed(&self) -> bool;
}

pub trait FunctionExpression: Function + Expression {}

pub trait UnaryExpression: Expression {
    fn get_operator(&self) -> &UnaryOperator;
    fn get_prefix(&self) -> bool;
    fn get_argument(&self) -> &node::Expression;
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOperator {
    Minus,
    Plus,
    Exclamation,
    Tilde,
    Typeof,
    Void,
    Delete,
}

pub trait UpdateExpression: Expression {
    fn get_operator(&self) -> &UpdateOperator;
    fn get_argument(&self) -> &node::Expression;
    fn get_prefix(&self) -> bool;
}

#[derive(Debug, Clone, PartialEq)]
pub enum UpdateOperator {
    PlusPlus,
    MinusMinus,
}

pub trait BinaryExpression: Expression {
    fn get_operator(&self) -> &BinaryOperator;
    fn get_left(&self) -> &node::Expression;
    fn get_right(&self) -> &node::Expression;
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOperator {
    EqualEqual,
    ExclamationEqual,
    EqualEqualEqual,
    ExclamationEqualEqual,
    Less,
    LessEqual,
    More,
    MoreEqual,
    LessLess,
    MoreMore,
    MoreMoreMore,
    Plus,
    Minus,
    Times,
    Slash,
    Percent,
    Pipe,
    Carret,
    And,
    In,
    Instanceof,
    StarStar,
}

pub trait AssignmentExpression: Expression {
    fn get_operator(&self) -> &AssignmentOperator;
    fn get_left(&self) -> &Pattern;
    fn get_right(&self) -> &Expression;
}

#[derive(Debug, Clone, Copy)]
pub enum AssignmentOperator {
    Equal,
    PlusEqual,
    MinusEqual,
    TimesEqual,
    SlashEqual,
    PercentEqual,
    LessLessEqual,
    MoreMoreEqual,
    MoreMoreMoreEqual,
    PipeEqual,
    CaretEqual,
    AndEqual,
    StarStarEqual,
}

pub trait LogicalExpression: Expression {
    fn get_operator(&self) -> LogicalOperator;
    fn get_left(&self) -> &Expression;
    fn get_right(&self) -> &Expression;
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum LogicalOperator {
    Or,
    And,
}

trait MemberExpression: Expression + Pattern {
    fn get_object(&self) -> &MemberExpressionObject;
    fn get_property(&self) -> &Expression;
    fn get_computed(&self) -> bool;
}

enum MemberExpressionObject {
    Expression(Box<Expression>),
    Super(Box<Super>),
}

trait ConditionalExpression: Expression {
    fn get_test(&self) -> &Expression;
    fn get_alternate(&self) -> &Expression;
    fn get_consequent(&self) -> &Expression;
}

trait CallExpression: Expression {
    fn get_callee(&self) -> &CallExpressionCallee;
    fn get_arguments(&self) -> &Vec<CallExpressionArgument>;
}

enum CallExpressionArgument {
    Expression(Box<Expression>),
    Super(Box<Super>),
}

enum CallExpressionCallee {
    Expression(Box<Expression>),
    Super(Box<Super>),
}

pub trait NewExpression: Expression {
    fn get_callee(&self) -> &node::Expression;
    fn get_arguments(&self) -> &Vec<NewExpressionArgument>;
}

#[derive(Debug, Clone)]
pub enum NewExpressionArgument {
    Expression(Box<node::Expression>),
    Super(Box<node::Super>),
}

pub trait SequenceExpression: Expression {
    fn get_expressions(&self) -> &Vec<node::Expression>;
}

pub trait Pattern: Node {}
mopafy!(Pattern);

pub trait Super: Node {}

pub trait SpreadElement: Node {
    fn get_argument(&self) -> &Expression;
}

pub trait ArrowFunctionExpression: Function + Expression {
    fn get_body(&self) -> &node::ArrowFunctionExpressionBody;
    fn get_expression(&self) -> bool;
}

trait YieldExpression: Expression {
    fn get_argument(&self) -> Option<&Expression>;
    fn get_delegate(&self) -> bool;
}

trait TemplateLiteral: Expression {
    fn get_quasis(&self) -> Vec<&TemplateElement>;
    fn get_expressions(&self) -> Vec<&Expression>;
}

trait TaggedTemplateExpression: Expression {
    fn get_tag(&self) -> &Expression;
    fn get_quasi(&self) -> &TemplateLiteral;
}

struct TemplateElementValue {
    cooked: Option<String>,
    raw: String,
}
trait TemplateElement: Node {
    fn get_tail(&self) -> bool;
    fn get_value(&self) -> &TemplateElementValue;
}

pub trait AssignmentProperty: Property {
    fn get_value(&self) -> &Pattern;
    fn get_kind(&self) -> &PropertyKind {
        &PropertyKind::Init
    }
    fn get_method(&self) -> bool {
        false
    }
}

#[derive(Debug, Clone)]
pub enum PropertyKind {
    Init,
}

pub trait ObjectPattern: Pattern {
    fn get_properties(&self) -> &Vec<node::ObjectPatternProperty>;
}

pub trait ArrayPattern: Pattern {
    fn get_elements(&self) -> &Vec<Option<node::Pattern>>;
}

pub trait RestElement: Pattern {
    fn get_argument(&self) -> &Pattern;
}

pub trait AssignmentPattern: Pattern {
    fn get_left(&self) -> &Pattern;
    fn get_right(&self) -> &Expression;
}

pub trait Class: Node {
    fn get_id(&self) -> &Option<node::Identifier>;
    fn get_super_class(&self) -> &Option<node::Expression>;
    fn get_body(&self) -> &ClassBody;
}

pub trait ClassBody: Node {
    fn get_body(&self) -> &Vec<node::MethodDefinition>;
}

pub trait MethodDefinition: Node {
    fn get_key(&self) -> &Expression;
    fn get_value(&self) -> &FunctionExpression;
    fn get_kind(&self) -> &MethodDefinitionKind;
    fn get_computed(&self) -> bool;
    fn get_static(&self) -> bool;
}
mopafy!(MethodDefinition);

#[derive(Debug, Copy, Clone, PartialEq)]
pub enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}

pub trait ClassDeclaration: Class + Declaration {
    fn get_id(&self) -> &Identifier;
}

pub trait ClassExpression: Class + Expression {}

trait MetaProperty: Expression {
    fn get_meta(&self) -> &Identifier;
    fn get_property(&self) -> &Identifier;
}

trait ModuleDeclaration: Node {}

trait ModuleSpecifier: Node {
    fn get_local(&self) -> &Identifier;
}

trait ImportDeclaration: ModuleDeclaration {
    fn get_specifiers(&self) -> Vec<&ImportDeclarationSpecifier>;
    fn get_source(&self) -> &Literal;
}

enum ImportDeclarationSpecifier {
    ImportSpecifier(Box<ImportSpecifier>),
    ImportDefaultSpecifier(Box<ImportDefaultSpecifier>),
    ImportNamespaceSpecifier(Box<ImportNamespaceSpecifier>),
}

trait ImportSpecifier: ModuleSpecifier {
    fn get_imported(&self) -> &Identifier;
}

trait ImportDefaultSpecifier: ModuleSpecifier {}

trait ImportNamespaceSpecifier: ModuleSpecifier {}

trait ExportNamedDeclaration: ModuleDeclaration {
    fn get_declaration(&self) -> &Option<Box<Declaration>>;
    fn get_specifiers(&self) -> &Vec<Box<ExportSpecifier>>;
    fn get_source(&self) -> &Option<Box<Literal>>;
}

trait ExportSpecifier: ModuleSpecifier {
    fn get_exported(&self) -> &Identifier;
}

trait ExportDefaultDeclaration: ModuleDeclaration {
    fn get_declaration(&self) -> &ExportDefaultDeclarationDeclaration;
}

enum ExportDefaultDeclarationDeclaration {
    Declaration(Box<Declaration>),
    Expression(Box<Expression>),
}

trait ExportAllDeclaration: ModuleDeclaration {
    fn get_source(&self) -> &Literal;
}

trait AwaitExpression: Expression {
    fn get_argument(&self) -> &Expression;
}
