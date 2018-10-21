use crate::lexer::token::Number;
use crate::parser::estree;
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
    id: Box<estree::Pattern>,
    init: Option<Box<estree::Expression>>,
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
}
impl estree::Node for VariableDeclarator {}

#[derive(Debug)]
pub struct VariableDeclaration {
    pub kind: estree::VariableDeclarationKind,
    pub declarations: Vec<Box<estree::VariableDeclarator>>,
}

impl estree::VariableDeclaration for VariableDeclaration {
    fn get_declarations(&self) -> &Vec<Box<estree::VariableDeclarator>> {
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

impl estree::BlockStatement for BlockStatement {
    fn get_body(&self) -> &Vec<Box<estree::Statement>> {
        &self.body
    }
}
impl estree::Statement for BlockStatement {}
impl estree::Node for BlockStatement {}

#[derive(Debug)]
pub struct EmptyStatement {}

impl estree::EmptyStatement for EmptyStatement {}
impl estree::Statement for EmptyStatement {}
impl estree::Node for EmptyStatement {}
