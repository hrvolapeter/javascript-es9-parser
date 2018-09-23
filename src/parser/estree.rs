/// AST tree interfaces as specified in ESTree standart
/// https://github.com/estree/estree/

trait Node {}

struct SourceLocation {
    source: Option<String>,
    start: Position,
    end: Position,
}

struct Position {
    line: i32,   // >= 1
    column: i32, // >= 0
}

trait Identifier: Expression + Pattern {
    fn get_name(&self) -> &str;
}

trait Literal: Expression {}
enum LiteralS {
    StringLiteral,
    BooleanLiteral,
    NullLiteral,
    NumberLiteral,
    RegExpLiteral,
}

struct StringLiteral {}

struct BooleanLiteral {}

struct NullLiteral {}

struct NumberLiteral {}

struct Regex {}

trait RegExpLiteral: Literal {
    fn get_regex(&self) -> &Regex;
}

trait Program: Node {
    fn get_body(&self) -> &Vec<ProgramBody>;
    fn get_source_type(&self) -> &ProgramSourceType;
}

enum ProgramSourceType {
    Script,
    Module,
}

enum ProgramBody {
    ProgramDirective,
    ProgramStatement,
}

trait Function: Node {
    fn get_id(&self) -> &Option<Box<Identifier>>;
    fn get_params(&self) -> &Vec<Box<Pattern>>;
    fn get_body(&self) -> &FunctionBody;
    fn get_generator(&self) -> bool;
    fn get_async(&self) -> bool;
}

trait Statement: Node {}

trait ExpressionStatement: Statement {
    fn get_expression(&self) -> &Box<Expression>;
}

trait Directive: Node {
    fn get_expression(&self) -> &Literal;
    fn get_directive(&self) -> &String;
}

trait BlockStatement: Statement {
    fn get_body(&self) -> &Vec<Box<Statement>>;
}

trait FunctionBody: BlockStatement {
    fn get_body(&self) -> &Vec<FunctionBodyEnum>;
}

enum FunctionBodyEnum {
    Directive,
    Statement,
}

trait EmptyStatement: Statement {}

trait DebuggerStatement: Statement {}

trait WithStatement: Statement {
    fn get_object(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Statement;
}

trait ReturnStatement: Statement {
    fn get_argument(&self) -> &Option<Box<Expression>>;
}

trait LabeledStatement: Statement {
    fn get_label(&self) -> &Identifier;
    fn get_body(&self) -> &Statement;
}

trait BreakStatement: Statement {
    fn get_label(&self) -> &Option<Box<Identifier>>;
}

trait ContinueStatement: Statement {
    fn get_label(&self) -> &Option<Box<Identifier>>;
}

trait IfStatement: Statement {
    fn get_test(&self) -> &Box<Expression>;
    fn get_consequent(&self) -> &Statement;
    fn get_alternate(&self) -> &Option<Box<Statement>>;
}

trait SwitchStatement: Statement {
    fn get_discriminant(&self) -> &Box<Expression>;
    fn get_cases(&self) -> &Vec<Box<SwitchCase>>;
}

trait SwitchCase: Node {
    fn get_test(&self) -> &Option<Box<Expression>>;
    fn get_consequent(&self) -> &Vec<Box<Statement>>;
}

trait ThrowStatement: Statement {
    fn get_argument(&self) -> Box<Expression>;
}

trait TryStatement: Statement {
    fn get_block(&self) -> &BlockStatement;
    fn get_handler(&self) -> &Option<Box<CatchClause>>;
    fn get_finalizer(&self) -> &Option<Box<BlockStatement>>;
}

trait CatchClause: Node {
    fn get_param(&self) -> &Box<Pattern>;
    fn get_body(&self) -> &BlockStatement;
}

trait WhileStatement: Statement {
    fn get_test(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Statement;
}

trait DoWhileStatement: Statement {
    fn get_body(&self) -> &Statement;
    fn get_test(&self) -> &Box<Expression>;
}

trait ForStatement: Statement {
    fn get_init(&self) -> &Option<ForStatementInit>;
    fn get_test(&self) -> &Option<Box<Expression>>;
    fn get_update(&self) -> &Option<Box<Expression>>;
    fn get_body(&self) -> &Statement;
}

enum ForStatementInit {
    VariableDeclaration,
    Expression,
}

trait ForInStatement: Statement {
    fn get_left(&self) -> &Box<ForInStatementLeft>;
    fn get_right(&self) -> &Box<Expression>;
    fn get_body(&self) -> &Statement;
}

enum ForInStatementLeft {
    VariableDeclaration,
    Pattern,
}

trait ForOfStatement: ForInStatement {
    fn get_await(&self) -> bool;
}

trait Declaration: Statement {}

// Declarations
trait FunctionDeclaration: Function + Declaration {
    fn get_id(&self) -> &Identifier;
}

trait VariableDeclaration: Declaration {
    fn get_declarations(&self) -> &Vec<Box<VariableDeclarator>>;
    fn get_kind(&self) -> &VariableDeclarationKind;
}

enum VariableDeclarationKind {
    Const,
    Let,
    Var,
}

trait VariableDeclarator: Node {
    fn get_id(&self) -> &Box<Pattern>;
    fn get_init(&self) -> &Option<Box<Expression>>;
}

trait Expression: Node {}

trait ThisExpression: Expression {}

trait ArrayExpression: Expression {
    fn get_elements(&self) -> &Vec<Option<ArrayExpressionElement>>;
}

enum ArrayExpressionElement {
    Expression(Box<Expression>),
    SpreadElement(Box<SpreadElement>),
}

trait ObjectExpression: Expression {
    fn get_properties(&self) -> &Vec<ObjectExpressionProperty>;
}

enum ObjectExpressionProperty {
    Property(Box<Property>),
    SpreadElement(Box<SpreadElement>),
}

trait Property: Node {
    fn get_key(&self) -> &Box<Expression>;
    fn get_value(&self) -> &Box<Expression>;
    fn get_kind(&self) -> &str;
    fn get_method(&self) -> bool;
    fn get_shorthand(&self) -> bool;
    fn get_computed(&self) -> bool;
}

trait FunctionExpression: Function + Expression {}

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

trait AssignmentExpression: Expression {
    fn get_operator(&self) -> &AssignmentOperator;
    fn get_left(&self) -> &Box<Pattern>;
    fn get_right(&self) -> &Box<Expression>;
}

enum AssignmentOperator {
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

trait SequenceExpression: Expression {
    fn get_expressions(&self) -> &Vec<Box<Expression>>;
}

trait Pattern: Node {}

trait Super: Node {}

trait SpreadElement: Node {
    fn get_argument(&self) -> &Box<Expression>;
}

trait ArrowFunctionExpression: Function + Expression {
    fn get_body(&self) -> &ArrowFunctionExpressionBody;
    fn get_expression(&self) -> bool;
}

enum ArrowFunctionExpressionBody {
    FunctionBody(Box<FunctionBody>),
    Expression(Box<Expression>),
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

trait AssignmentProperty: Property {
    fn get_value(&self) -> &Box<Pattern>;
    fn get_kind(&self) -> &AssignmentPropertyKind {
        &AssignmentPropertyKind::Init
    }
    fn get_method(&self) -> bool {
        false
    }
}

enum AssignmentPropertyKind {
    Init,
}

trait ObjectPattern: Pattern {
    fn get_properties(&self) -> &Vec<ObjectPatternProperty>;
}

enum ObjectPatternProperty {
    AssignmentProperty(Box<AssignmentProperty>),
    RestElement(Box<RestElement>),
}

trait ArrayPattern: Pattern {
    fn get_elements(&self) -> &Vec<Option<Box<Pattern>>>;
}

trait RestElement: Pattern {
    fn get_argument(&self) -> &Box<Pattern>;
}

trait AssignmentPattern: Pattern {
    fn get_left(&self) -> &Box<Pattern>;
    fn get_right(&self) -> &Box<Expression>;
}

trait Class: Node {
    fn get_id(&self) -> &Option<Box<Identifier>>;
    fn get_super_class(&self) -> &Option<Box<Expression>>;
    fn get_body(&self) -> &Box<ClassBody>;
}

trait ClassBody: Node {
    fn get_body(&self) -> &Vec<Box<MethodDefinition>>;
}

trait MethodDefinition: Node {
    fn get_key(&self) -> &Box<Expression>;
    fn get_value(&self) -> &Box<FunctionExpression>;
    fn get_kind(&self) -> &MethodDefinitionKind;
    fn get_computed(&self) -> bool;
    fn get_static(&self) -> bool;
}

enum MethodDefinitionKind {
    Constructor,
    Method,
    Get,
    Set,
}

trait ClassDeclaration: Class + Declaration {
    fn get_id(&self) -> &Box<Identifier>;
}

trait ClassExpression: Class + Expression {}

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
