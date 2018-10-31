use crate::lexer;
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

    fn box_clone(&self) -> Box<Identifier>;
}

pub trait Literal: Expression {}

struct Regex {}

trait RegExpLiteral: Literal {
    fn get_regex(&self) -> &Regex;
}
pub trait Program: Node {
    fn get_body(&self) -> &Vec<ProgramBody>;
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

#[derive(Debug)]
pub enum ProgramBody {
    ProgramDirective,
    ProgramStatement(Box<Statement>),
}

pub trait Function: Node {
    fn get_id(&self) -> &Option<Box<Identifier>>;
    fn get_params(&self) -> &Vec<Box<Pattern>>;
    fn get_body(&self) -> &Box<FunctionBody>;
    fn get_generator(&self) -> bool;
    fn get_async(&self) -> bool;
}

pub trait Statement: Node {
    fn box_clone(&self) -> Box<Statement>;
}
mopafy!(Statement);

pub trait ExpressionStatement: Statement {
    fn get_expression(&self) -> &Box<Expression>;
}

pub trait Directive: Node {
    fn get_expression(&self) -> &Literal;
    fn get_directive(&self) -> &String;

    fn box_clone(&self) -> Box<Directive>;
}

pub trait BlockStatement: Statement {
    fn get_body(&self) -> &Vec<Box<Statement>>;

    fn box_clone(&self) -> Box<BlockStatement>;
}

pub trait FunctionBody: BlockStatement {
    fn get_body(&self) -> &Vec<FunctionBodyEnum>;

    fn box_clone(&self) -> Box<FunctionBody>;
}

#[derive(Debug)]
pub enum FunctionBodyEnum {
    Directive(Box<Directive>),
    Statement(Box<Statement>),
}

impl Clone for FunctionBodyEnum {
    fn clone(&self) -> Self {
        match self {
            FunctionBodyEnum::Directive(p) => {
                FunctionBodyEnum::Directive(Directive::box_clone(&**p))
            }
            FunctionBodyEnum::Statement(p) => {
                FunctionBodyEnum::Statement(Statement::box_clone(&**p))
            }
        }
    }
}

pub trait EmptyStatement: Statement {}

pub trait DebuggerStatement: Statement {}

pub trait WithStatement: Statement {
    fn get_object(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Box<Statement>;
}

pub trait ReturnStatement: Statement {
    fn get_argument(&self) -> &Option<Box<Expression>>;
}

pub trait LabeledStatement: Statement {
    fn get_label(&self) -> &Box<Identifier>;
    fn get_body(&self) -> &Box<Statement>;
}

pub trait BreakStatement: Statement {
    fn get_label(&self) -> &Option<Box<Identifier>>;
}

pub trait ContinueStatement: Statement {
    fn get_label(&self) -> &Option<Box<Identifier>>;
}

pub trait IfStatement: Statement {
    fn get_test(&self) -> &Box<Expression>;
    fn get_consequent(&self) -> &Box<Statement>;
    fn get_alternate(&self) -> &Option<Box<Statement>>;
}

pub trait SwitchStatement: Statement {
    fn get_discriminant(&self) -> &Box<Expression>;
    fn get_cases(&self) -> &Vec<Box<SwitchCase>>;
}

pub trait SwitchCase: Node {
    fn get_test(&self) -> &Option<Box<Expression>>;
    fn get_consequent(&self) -> &Vec<Box<Statement>>;

    fn box_clone(&self) -> Box<SwitchCase>;
}

pub trait ThrowStatement: Statement {
    fn get_argument(&self) -> &Box<Expression>;
}

pub trait TryStatement: Statement {
    fn get_block(&self) -> &Box<BlockStatement>;
    fn get_handler(&self) -> &Option<Box<CatchClause>>;
    fn get_finalizer(&self) -> &Option<Box<BlockStatement>>;
}

pub trait CatchClause: Node {
    fn get_param(&self) -> &Box<Pattern>;
    fn get_body(&self) -> &Box<BlockStatement>;

    fn box_clone(&self) -> Box<CatchClause>;
}

pub trait WhileStatement: Statement {
    fn get_test(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Box<Statement>;
}

pub trait DoWhileStatement: Statement {
    fn get_body(&self) -> &Box<Statement>;
    fn get_test(&self) -> &Box<Expression>;
}

pub trait ForStatement: Statement {
    fn get_init(&self) -> &Option<ForStatementInit>;
    fn get_test(&self) -> &Option<Box<Expression>>;
    fn get_update(&self) -> &Option<Box<Expression>>;
    fn get_body(&self) -> &Box<Statement>;
}

#[derive(Debug)]
pub enum ForStatementInit {
    VariableDeclaration(Box<VariableDeclaration>),
    Expression(Box<Expression>),
}

impl Clone for ForStatementInit {
    fn clone(&self) -> Self {
        match self {
            ForStatementInit::VariableDeclaration(r) => {
                ForStatementInit::VariableDeclaration(VariableDeclaration::box_clone(&**r))
            }
            ForStatementInit::Expression(r) => ForStatementInit::Expression(r.box_clone()),
        }
    }
}

pub trait ForInStatement: Statement {
    fn get_left(&self) -> &ForInStatementLeft;
    fn get_right(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Box<Statement>;
}

#[derive(Debug)]
pub enum ForInStatementLeft {
    VariableDeclaration(Box<VariableDeclaration>),
    Pattern(Box<Pattern>),
}

impl Clone for ForInStatementLeft {
    fn clone(&self) -> Self {
        match self {
            ForInStatementLeft::VariableDeclaration(r) => {
                ForInStatementLeft::VariableDeclaration(VariableDeclaration::box_clone(&**r))
            }
            ForInStatementLeft::Pattern(r) => ForInStatementLeft::Pattern(r.box_clone()),
        }
    }
}

pub trait ForOfStatement: ForInStatement {
    fn get_await(&self) -> bool;
}

pub trait Declaration: Statement {
    fn box_clone(&self) -> Box<Declaration>;
}

// Declarations
pub trait FunctionDeclaration: Function + Declaration {
    fn get_id(&self) -> &Box<Identifier>;
}

pub trait VariableDeclaration: Declaration {
    fn get_declarations(&self) -> &Vec<Box<VariableDeclarator>>;
    fn get_kind(&self) -> &VariableDeclarationKind;

    fn box_clone(&self) -> Box<VariableDeclaration>;
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
    fn get_id(&self) -> &Box<Pattern>;
    fn get_init(&self) -> &Option<Box<Expression>>;

    fn box_clone(&self) -> Box<VariableDeclarator>;
}

pub trait Expression: Node {
    fn box_clone(&self) -> Box<Expression>;
}
mopafy!(Expression);

trait ThisExpression: Expression {}

trait ArrayExpression: Expression {
    fn get_elements(&self) -> &Vec<Option<ArrayExpressionElement>>;
}

enum ArrayExpressionElement {
    Expression(Box<Expression>),
    SpreadElement(Box<SpreadElement>),
}

pub trait ObjectExpression: Expression {
    fn get_properties(&self) -> &Vec<ObjectExpressionProperty>;
}

#[derive(Debug)]
pub enum ObjectExpressionProperty {
    Property(Box<Property>),
    SpreadElement(Box<SpreadElement>),
}

impl Clone for ObjectExpressionProperty {
    fn clone(&self) -> Self {
        match self {
            ObjectExpressionProperty::Property(r) => {
                ObjectExpressionProperty::Property(r.box_clone())
            }
            ObjectExpressionProperty::SpreadElement(r) => {
                ObjectExpressionProperty::SpreadElement(r.box_clone())
            }
        }
    }
}

pub trait Property: Node {
    fn get_key(&self) -> &Box<Expression>;
    fn get_value(&self) -> &Box<Expression>;
    fn get_kind(&self) -> String;
    fn get_method(&self) -> bool;
    fn get_shorthand(&self) -> bool;
    fn get_computed(&self) -> bool;
    fn box_clone(&self) -> Box<Property>;
}

pub trait FunctionExpression: Function + Expression {
    fn box_clone(&self) -> Box<FunctionExpression>;
}

trait UnaryExpression: Expression {
    fn get_operator(&self) -> &UnaryOperator;
    fn get_prefix(&self) -> bool;
    fn get_argument(&self) -> &Box<Expression>;
}

enum UnaryOperator {
    Minus,
    Plus,
    Exclamation,
    Tilde,
    Typeof,
    Void,
    Delete,
}

trait UpdateExpression: Expression {
    fn get_operator(&self) -> &UpdateOperator;
    fn get_argument(&self) -> &Box<Expression>;
    fn get_prefix(&self) -> bool;
}

enum UpdateOperator {
    PlusPlus,
    MinusMinus,
}

trait BinaryExpression: Expression {
    fn get_operator(&self) -> &BinaryOperator;
    fn get_left(&self) -> &Box<Expression>;
    fn get_right(&self) -> &Box<Expression>;
}

enum BinaryOperator {
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
    fn get_left(&self) -> &Box<Pattern>;
    fn get_right(&self) -> &Box<Expression>;
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

trait LogicalExpression: Expression {
    fn get_operator(&self) -> &LogicalOperator;
    fn get_left(&self) -> &Box<Expression>;
    fn get_right(&self) -> &Box<Expression>;
}

enum LogicalOperator {
    Or,
    And,
}

trait MemberExpression: Expression + Pattern {
    fn get_object(&self) -> &MemberExpressionObject;
    fn get_property(&self) -> &Box<Expression>;
    fn get_computed(&self) -> bool;
}

enum MemberExpressionObject {
    Expression(Box<Expression>),
    Super(Box<Super>),
}

trait ConditionalExpression: Expression {
    fn get_test(&self) -> &Box<Expression>;
    fn get_alternate(&self) -> &Box<Expression>;
    fn get_consequent(&self) -> &Box<Expression>;
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

trait NewExpression: Expression {
    fn get_callee(&self) -> &Box<Expression>;
    fn get_arguments(&self) -> &Vec<NewExpressionArgument>;
}

enum NewExpressionArgument {
    Expression(Box<Expression>),
    Super(Box<Super>),
}

pub trait SequenceExpression: Expression {
    fn get_expressions(&self) -> &Vec<Box<Expression>>;
}

pub trait Pattern: Node {
    fn box_clone(&self) -> Box<Pattern>;
}
mopafy!(Pattern);

trait Super: Node {}

pub trait SpreadElement: Node {
    fn get_argument(&self) -> &Box<Expression>;
    fn box_clone(&self) -> Box<SpreadElement>;
}

pub trait ArrowFunctionExpression: Function + Expression {
    fn get_body(&self) -> &ArrowFunctionExpressionBody;
    fn get_expression(&self) -> bool;
}

#[derive(Debug)]
pub enum ArrowFunctionExpressionBody {
    FunctionBody(Box<FunctionBody>),
    Expression(Box<Expression>),
}

impl Clone for ArrowFunctionExpressionBody {
    fn clone(&self) -> Self {
        match self {
            ArrowFunctionExpressionBody::FunctionBody(p) => {
                ArrowFunctionExpressionBody::FunctionBody(FunctionBody::box_clone(&**p))
            }
            ArrowFunctionExpressionBody::Expression(p) => {
                ArrowFunctionExpressionBody::Expression(Expression::box_clone(&**p))
            }
        }
    }
}

trait YieldExpression: Expression {
    fn get_argument(&self) -> &Option<Box<Expression>>;
    fn get_delegate(&self) -> bool;
}

trait TemplateLiteral: Expression {
    fn get_quasis(&self) -> &Vec<Box<TemplateElement>>;
    fn get_expressions(&self) -> &Vec<Box<Expression>>;
}

trait TaggedTemplateExpression: Expression {
    fn get_tag(&self) -> &Box<Expression>;
    fn get_quasi(&self) -> &Box<TemplateLiteral>;
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
    fn get_value(&self) -> &Box<Pattern>;
    fn get_kind(&self) -> &AssignmentPropertyKind {
        &AssignmentPropertyKind::Init
    }
    fn get_method(&self) -> bool {
        false
    }
    fn box_clone(&self) -> Box<AssignmentProperty>;
}

#[derive(Debug)]
pub enum AssignmentPropertyKind {
    Init,
}

pub trait ObjectPattern: Pattern {
    fn get_properties(&self) -> &Vec<ObjectPatternProperty>;
}

#[derive(Debug)]
pub enum ObjectPatternProperty {
    AssignmentProperty(Box<AssignmentProperty>),
    RestElement(Box<RestElement>),
}

impl Clone for ObjectPatternProperty {
    fn clone(&self) -> Self {
        match self {
            ObjectPatternProperty::AssignmentProperty(p) => {
                ObjectPatternProperty::AssignmentProperty(AssignmentProperty::box_clone(&**p))
            }
            ObjectPatternProperty::RestElement(p) => {
                ObjectPatternProperty::RestElement(RestElement::box_clone(&**p))
            }
        }
    }
}

pub trait ArrayPattern: Pattern {
    fn get_elements(&self) -> &Vec<Option<Box<Pattern>>>;
}

pub trait RestElement: Pattern {
    fn get_argument(&self) -> &Box<Pattern>;
    fn box_clone(&self) -> Box<RestElement>;
}

pub trait AssignmentPattern: Pattern {
    fn get_left(&self) -> &Box<Pattern>;
    fn get_right(&self) -> &Box<Expression>;
}

pub trait Class: Node {
    fn get_id(&self) -> &Option<Box<Identifier>>;
    fn get_super_class(&self) -> &Option<Box<Expression>>;
    fn get_body(&self) -> &Box<ClassBody>;
}

pub trait ClassBody: Node {
    fn get_body(&self) -> &Vec<Box<MethodDefinition>>;

    fn box_clone(&self) -> Box<ClassBody>;
}

pub trait MethodDefinition: Node {
    fn get_key(&self) -> &Box<Expression>;
    fn get_value(&self) -> &Box<FunctionExpression>;
    fn get_kind(&self) -> &MethodDefinitionKind;
    fn get_computed(&self) -> bool;
    fn get_static(&self) -> bool;

    fn box_clone(&self) -> Box<MethodDefinition>;
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
    fn get_id(&self) -> &Box<Identifier>;
}

pub trait ClassExpression: Class + Expression {}

trait MetaProperty: Expression {
    fn get_meta(&self) -> &Box<Identifier>;
    fn get_property(&self) -> &Box<Identifier>;
}

trait ModuleDeclaration: Node {}

trait ModuleSpecifier: Node {
    fn get_local(&self) -> &Box<Identifier>;
}

trait ImportDeclaration: ModuleDeclaration {
    fn get_specifiers(&self) -> &Vec<ImportDeclarationSpecifier>;
    fn get_source(&self) -> &Box<Literal>;
}

enum ImportDeclarationSpecifier {
    ImportSpecifier(Box<ImportSpecifier>),
    ImportDefaultSpecifier(Box<ImportDefaultSpecifier>),
    ImportNamespaceSpecifier(Box<ImportNamespaceSpecifier>),
}

trait ImportSpecifier: ModuleSpecifier {
    fn get_imported(&self) -> &Box<Identifier>;
}

trait ImportDefaultSpecifier: ModuleSpecifier {}

trait ImportNamespaceSpecifier: ModuleSpecifier {}

trait ExportNamedDeclaration: ModuleDeclaration {
    fn get_declaration(&self) -> &Option<Box<Declaration>>;
    fn get_specifiers(&self) -> &Vec<Box<ExportSpecifier>>;
    fn get_source(&self) -> &Option<Box<Literal>>;
}

trait ExportSpecifier: ModuleSpecifier {
    fn get_exported(&self) -> &Box<Identifier>;
}

trait ExportDefaultDeclaration: ModuleDeclaration {
    fn get_declaration(&self) -> &ExportDefaultDeclarationDeclaration;
}

enum ExportDefaultDeclarationDeclaration {
    Declaration(Box<Declaration>),
    Expression(Box<Expression>),
}

trait ExportAllDeclaration: ModuleDeclaration {
    fn get_source(&self) -> &Box<Literal>;
}

trait AwaitExpression: Expression {
    fn get_argument(&self) -> &Box<Expression>;
}
