use crate::{estree, node};

pub trait StaticSemantics {
    fn check_semantics(&self) -> Result<(), &str>;
}

macro_rules! empty_semantics (
  ($id: path) => (
    impl StaticSemantics for $id {
        fn check_semantics(&self) -> Result<(), &str> {
            Ok(())
        }
    }
  );
);

#[inline]
fn semantics_vec<T: StaticSemantics>(v: &Vec<T>) -> Result<(), &str> {
    for i in v {
        i.check_semantics()?;
    }
    Ok(())
}

impl StaticSemantics for node::Program {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body)
    }
}

impl StaticSemantics for node::ProgramBody {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::ProgramBody::ProgramStatement(stmt) => stmt.check_semantics()?,
            _ => {}
        }
        Ok(())
    }
}

impl StaticSemantics for node::Statement {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::Statement::VariableDeclaration(stmt) => stmt.check_semantics()?,
            node::Statement::BlockStatement(stmt) => stmt.check_semantics()?,
            node::Statement::EmptyStatement(stmt) => stmt.check_semantics()?,
            node::Statement::FunctionBody(stmt) => stmt.check_semantics()?,
            node::Statement::IfStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ExpressionStatement(stmt) => stmt.check_semantics()?,
            node::Statement::DoWhileStatement(stmt) => stmt.check_semantics()?,
            node::Statement::WhileStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ForStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ForInStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ForOfStatement(stmt) => stmt.check_semantics()?,
            node::Statement::SwitchStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ContinueStatement(stmt) => stmt.check_semantics()?,
            node::Statement::BreakStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ReturnStatement(stmt) => stmt.check_semantics()?,
            node::Statement::WithStatement(stmt) => stmt.check_semantics()?,
            node::Statement::LabeledStatement(stmt) => stmt.check_semantics()?,
            node::Statement::ThrowStatement(stmt) => stmt.check_semantics()?,
            node::Statement::TryStatement(stmt) => stmt.check_semantics()?,
            node::Statement::DebuggerStatement(stmt) => stmt.check_semantics()?,
            node::Statement::FunctionDeclaration(stmt) => stmt.check_semantics()?,
            node::Statement::ClassDeclaration(stmt) => stmt.check_semantics()?,
            node::Statement::Property(stmt) => stmt.check_semantics()?,
        };
        Ok(())
    }
}

impl StaticSemantics for node::Expression {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::Expression::StringLiteral(expr) => expr.check_semantics()?,
            node::Expression::BooleanLiteral(expr) => expr.check_semantics()?,
            node::Expression::NullLiteral(expr) => expr.check_semantics()?,
            node::Expression::UndefinedLiteral(expr) => expr.check_semantics()?,
            node::Expression::NumberLiteral(expr) => expr.check_semantics()?,
            node::Expression::Regex(expr) => expr.check_semantics()?,
            node::Expression::Identifier(expr) => expr.check_semantics()?,
            node::Expression::ObjectExpression(expr) => expr.check_semantics()?,
            node::Expression::ArrowFunctionExpression(expr) => expr.check_semantics()?,
            node::Expression::MemberExpression(expr) => expr.check_semantics()?,
            node::Expression::FunctionExpression(expr) => expr.check_semantics()?,
            node::Expression::AssignmentExpression(expr) => expr.check_semantics()?,
            node::Expression::ExpressionStatement(expr) => expr.check_semantics()?,
            node::Expression::SequenceExpression(expr) => expr.check_semantics()?,
            node::Expression::ThisExpression(expr) => expr.check_semantics()?,
            node::Expression::ArrayExpression(expr) => expr.check_semantics()?,
            node::Expression::LogicalExpression(expr) => expr.check_semantics()?,
            node::Expression::BinaryExpression(expr) => expr.check_semantics()?,
            node::Expression::UnaryExpression(expr) => expr.check_semantics()?,
            node::Expression::UpdateExpression(expr) => expr.check_semantics()?,
            node::Expression::NewExpression(expr) => expr.check_semantics()?,
            node::Expression::CallExpression(expr) => expr.check_semantics()?,
        };
        Ok(())
    }
}

impl StaticSemantics for node::Pattern {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::Pattern::ObjectPattern(pat) => pat.check_semantics()?,
            node::Pattern::ArrayPattern(pat) => pat.check_semantics()?,
            node::Pattern::Identifier(pat) => pat.check_semantics()?,
            node::Pattern::RestElement(pat) => pat.check_semantics()?,
            node::Pattern::AssignmentPattern(pat) => pat.check_semantics()?,
        };
        Ok(())
    }
}

empty_semantics!(node::ObjectPattern);
empty_semantics!(node::ArrayPattern);
empty_semantics!(node::RestElement);
empty_semantics!(node::AssignmentPattern);

empty_semantics!(node::StringLiteral);
empty_semantics!(node::BooleanLiteral);
empty_semantics!(node::NullLiteral);
empty_semantics!(node::UndefinedLiteral);
empty_semantics!(node::NumberLiteral);
empty_semantics!(node::Regex);
empty_semantics!(node::Identifier);
empty_semantics!(node::ObjectExpression);
empty_semantics!(node::ArrowFunctionExpression);

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-assignment-operators-static-semantics-early-errors
impl StaticSemantics for node::AssignmentExpression {
    fn check_semantics(&self) -> Result<(), &str> {
        let valid = match &*self.left {
            node::Pattern::ArrayPattern(_) | node::Pattern::ObjectPattern(_)
                if self.operator == estree::AssignmentOperator::Equal =>
            {
                true
            }
            t @ _ => IsValidSimpleAssignmentTarget(&t.clone().into()).is_ok(),
        };
        if !valid {
            return Err("Is not valid assignment target");
        }
        Ok(())
    }
}
empty_semantics!(node::SequenceExpression);
empty_semantics!(node::ThisExpression);
empty_semantics!(node::ArrayExpression);
empty_semantics!(node::LogicalExpression);
empty_semantics!(node::BinaryExpression);
empty_semantics!(node::UnaryExpression);

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-update-expressions-static-semantics-early-errors
impl StaticSemantics for node::UpdateExpression {
    fn check_semantics(&self) -> Result<(), &str> {
        IsValidSimpleAssignmentTarget(&(*self.argument).clone().into())?;
        Ok(())
    }
}

fn IsValidSimpleAssignmentTarget<'a>(n: &'a node::Node) -> Result<(), &'static str> {
    let res = match n {
        // TODO: Member expression should be allowed
        // node::Expression::MemberExpression(_) => true,
        node::Node::Identifier(_) => true,

        // TODO: Should check if ArrayExpression can be also derived with ArrayPattern rule
        node::Node::ArrayExpression(_) => true,
        node::Node::ObjectExpression(_) => true,
        _ => false,
    };
    if res {
        return Ok(());
    }
    Err("Is not valid assignment target")
}

empty_semantics!(node::NewExpression);

empty_semantics!(node::VariableDeclaration);

// http://www.ecma-international.org/ecma-262/9.0/index.html#sec-block-static-semantics-early-errors
impl StaticSemantics for node::BlockStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body)?;
        check_duplicate_names(&self.body)?;
        Ok(())
    }
}

fn check_duplicate_names(body: &Vec<node::Statement>) -> Result<(), &str> {
    let mut ids: Vec<&node::Pattern> = vec![];
    for stmt in body {
        match stmt {
            node::Statement::VariableDeclaration(var)
                if var.kind == estree::VariableDeclarationKind::Const
                    || var.kind == estree::VariableDeclarationKind::Let =>
            {
                for decl in &var.declarations {
                    ids.push(&decl.id);
                }
            }
            _ => {}
        }
    }
    for stmt in body {
        match stmt {
            node::Statement::VariableDeclaration(var) => {
                for decl in &var.declarations {
                    for id in &ids {
                        if *id == &decl.id && *id as *const _ != &decl.id as *const _ {
                            return Err("Variable is redeclared");
                        }
                    }
                }
            }
            _ => {}
        }
    }
    Ok(())
}

empty_semantics!(node::EmptyStatement);

impl StaticSemantics for node::FunctionBody {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body)?;
        Ok(())
    }
}

impl StaticSemantics for node::FunctionBodyEnum {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::FunctionBodyEnum::Statement(stmt) => stmt.check_semantics(),
        };
        Ok(())
    }
}

impl StaticSemantics for node::IfStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.consequent.check_semantics()?;
        if let Some(ref alt) = self.alternate {
            alt.check_semantics()?;
        }
        self.test.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::ExpressionStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.expression.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::DoWhileStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.test.check_semantics()?;
        self.body.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::WhileStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.test.check_semantics()?;
        self.body.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::ForStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        if let Some(ref test) = self.test {
            test.check_semantics()?;
        }
        self.body.check_semantics()?;
        if let Some(ref update) = self.update {
            update.check_semantics()?;
        }
        Ok(())
    }
}

impl StaticSemantics for node::ForInStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.body.check_semantics()?;
        self.left.check_semantics()?;
        self.right.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::ForInStatementLeft {
    fn check_semantics(&self) -> Result<(), &str> {
        match self {
            node::ForInStatementLeft::Pattern(pat) => pat.check_semantics()?,
            node::ForInStatementLeft::VariableDeclaration(var) => var.check_semantics()?,
        };
        Ok(())
    }
}

impl StaticSemantics for node::ForOfStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.body.check_semantics()?;
        self.left.check_semantics()?;
        self.right.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::SwitchStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.discriminant.check_semantics()?;
        Ok(())
    }
}

empty_semantics!(node::ContinueStatement);
empty_semantics!(node::BreakStatement);
empty_semantics!(node::ReturnStatement);

impl StaticSemantics for node::WithStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        self.body.check_semantics()?;
        self.object.check_semantics()?;
        Ok(())
    }
}

empty_semantics!(node::LabeledStatement);
empty_semantics!(node::ThrowStatement);
empty_semantics!(node::MemberExpression);
empty_semantics!(node::CallExpression);

impl StaticSemantics for node::TryStatement {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.block.body)?;
        if let Some(ref fin) = self.finalizer {
            fin.check_semantics()?;
        }
        if let Some(ref hnd) = self.handler {
            hnd.body.check_semantics()?;
        }
        Ok(())
    }
}

empty_semantics!(node::DebuggerStatement);

impl StaticSemantics for node::FunctionDeclaration {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body.body)?;
        semantics_vec(&self.params)?;
        Ok(())
    }
}

impl StaticSemantics for node::ClassDeclaration {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body.body)?;
        if let Some(ref super_class) = self.super_class {
            super_class.check_semantics()?;
        }
        Ok(())
    }
}

impl StaticSemantics for node::MethodDefinition {
    fn check_semantics(&self) -> Result<(), &str> {
        self.key.check_semantics()?;
        self.value.check_semantics()?;
        Ok(())
    }
}

impl StaticSemantics for node::FunctionExpression {
    fn check_semantics(&self) -> Result<(), &str> {
        semantics_vec(&self.body.body)?;
        semantics_vec(&self.params)?;
        Ok(())
    }
}

impl StaticSemantics for node::property {
    fn check_semantics(&self) -> Result<(), &str> {
        self.value.check_semantics()?;
        self.key.check_semantics()?;
        Ok(())
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::{javascript_lexer::Lexer, Parser};

    macro_rules! test (
        ($id: ident, $code:expr) => (
            #[test]
            fn $id() {
                let res = Parser::ast_tree(
                    Lexer::lex_tokens($code)
                        .unwrap(),
                    estree::ProgramSourceType::Script,
                );
                res.check_semantics().unwrap();
            }
        );
        ($id: ident, $code:expr, $m:meta) => (
            #[test]
            #[$m]
            fn $id() {
                let res = Parser::ast_tree(
                    Lexer::lex_tokens($code)
                        .unwrap(),
                    estree::ProgramSourceType::Script,
                );
                res.check_semantics().unwrap();
            }
        );
    );

    test!(assignment_plus_simple, "a += 1;");
    test!(assignment_equal_simple, "a = 1;");
    // TODO: Missing impl for converting ArrayExpression to ArrayPattern
    // In some cases it safe and required to be able to covnert from ArrayExpression
    // to ArrayPattern
    // test!(invalid_assignment_equal_simple, "[a] = 1;");
    test!(
        assignment_equal_array_expression,
        "[a, 1] = 1;",
        should_panic
    );
    test!(block_var_decl_simple, "{var a;let b;}");
    test!(block_var_decl_double_name, "{var a;var a;}");
    test!(block_var_decl_override, "{let a;var a;}", should_panic);
    test!(block_let_decl_override, "{let a;let a;}", should_panic);
    test!(
        block_const_decl_override,
        "{const a;const a;}",
        should_panic
    );
    test!(block_const_decl_nested, "{const a; {const a;}}");
}
