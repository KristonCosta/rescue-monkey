use crate::ast::Expression;
use crate::ast::Program;
use crate::ast::{BlockStatement, Statement};
use std::cmp::max;
use std::io::Write;

pub struct ASTPrinter<T: Write + Sized> {
    writer: T,
    padding: usize,
    indent_level: usize,
}

impl<T: Write + Sized> ASTPrinter<T> {
    pub fn new(writer: T) -> Self {
        Self {
            writer,
            padding: 2,
            indent_level: 0,
        }
    }

    pub fn indent(&mut self) {
        self.indent_level += 1;
    }

    pub fn dedent(&mut self) {
        self.indent_level = max(0, self.indent_level - 1);
    }

    pub fn write(&mut self, str: &str) {
        self.writer
            .write_all(" ".repeat(self.indent_level * self.padding).as_bytes())
            .expect("failed to write to writer");
        self.writer.write_all(str.as_bytes()).unwrap();
    }

    pub fn print(mut self, program: &Program) -> T {
        for statement in program.statements.iter() {
            self.visit_statement(statement);
        }
        self.writer
    }

    pub fn visit_statement(&mut self, statement: &Statement) {
        match &statement.inner {
            crate::ast::StatementEnum::Return(expr) => {
                self.write(statement.token.literal());
                if let Some(expr) = expr {
                    self.write(" ");
                    self.visit_expression(expr);
                }
            }
            crate::ast::StatementEnum::Expression(expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.visit_expression(expr);
                }
            }
            crate::ast::StatementEnum::Let(l) => {
                self.write(statement.token.literal());
                self.write(" ");
                self.write(&l.identifier.value);
                self.write(" = ");
                if let Some(expr) = l.expr.as_ref() {
                    self.visit_expression(expr);
                }
            }
            crate::ast::StatementEnum::Block(statements) => match statements {
                Some(statements) => {
                    self.visit_block_statement(statements);
                }
                None => {
                    self.visit_block_statement(&Vec::new());
                }
            },
        }
    }

    fn visit_block_statement(&mut self, statements: &BlockStatement) {
        self.write("{\n");
        self.indent();

        for statement in statements {
            self.visit_statement(statement);
        }
        self.dedent();
        self.write("\n");
        self.write("}");
    }

    fn visit_expression(&mut self, expression: &Expression) {
        match &expression.inner {
            crate::ast::ExpressionEnum::Identifier(val) => self.write(&val.value),
            crate::ast::ExpressionEnum::IntegerLiteral(val) => self.write(&val.value.to_string()),
            crate::ast::ExpressionEnum::StringLiteral(val) => self.write(&val.value),
            crate::ast::ExpressionEnum::ArrayLiteral(val) => {
                self.write("[");
                for (index, element) in val.elements.iter().enumerate() {
                    if index != 0 {
                        self.write(", ");
                    }
                    self.visit_expression(element);
                }
                self.write("]");
            }
            crate::ast::ExpressionEnum::Boolean(val) => self.write(&val.value.to_string()),
            crate::ast::ExpressionEnum::HashLiteral(val) => {
                self.write("{");
                for (index, (key, value)) in val.pairs.iter().enumerate() {
                    if index != 0 {
                        self.write(", ");
                    }
                    self.visit_expression(key);
                    self.write(":");
                    self.visit_expression(value);
                }
                self.write("}");
            }
            crate::ast::ExpressionEnum::Prefix(val) => {
                self.write("(");
                self.write(val.operator.value());
                self.visit_expression(&val.left);
                self.write(")");
            }
            crate::ast::ExpressionEnum::Infix(infix) => {
                self.write("(");
                self.visit_expression(&infix.left);
                self.write(" ");
                self.write(infix.op.value());
                self.write(" ");
                self.visit_expression(&infix.right);
                self.write(")");
            }
            crate::ast::ExpressionEnum::Index(expr) => {
                self.write("(");
                self.visit_expression(&expr.indexable);
                self.write("[");
                self.visit_expression(&expr.index);
                self.write("])");
            }
            crate::ast::ExpressionEnum::If(expr) => {
                self.write("if ");
                self.visit_expression(&expr.condition);
                self.write(" ");
                self.visit_block_statement(&expr.true_statement);
                if let Some(false_statement) = &expr.false_statement {
                    self.write(" else ");
                    self.visit_block_statement(false_statement);
                }
            }
            crate::ast::ExpressionEnum::While(expr) => {
                self.write("while ");
                self.visit_block_statement(expr);
            }
            crate::ast::ExpressionEnum::FunctionLiteral(expr) => {
                self.write(&expression.token.literal());
                self.write("(");
                for (index, argument) in expr.identifiers.iter().enumerate() {
                    if index != 0 {
                        self.write(", ");
                    }
                    self.write(&argument.value);
                }
                self.write(") ");
                self.visit_block_statement(&expr.block);
            }
            crate::ast::ExpressionEnum::Call(expr) => {
                self.visit_expression(&expr.callable);
                self.write("(");
                for (index, argument) in expr.args.iter().enumerate() {
                    if index != 0 {
                        self.write(", ");
                    }
                    self.visit_expression(argument);
                }
                self.write(")");
            }
        }
    }
}
