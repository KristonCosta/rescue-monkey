use crate::ast::ArrayLiteral;
use crate::ast::Call;
use crate::ast::If;
use crate::ast::Index;
use crate::ast::Infix;
use crate::ast::Let;
use crate::ast::Prefix;
use crate::lexer::TokenStream;
use crate::{
    ast::{self, Identifier, Program},
    token::{Token, TokenType},
};
use ast::{
    BlockStatement, Expression, FunctionLiteral, HashLiteral, Operator, Statement, StatementEnum,
    ValueExpression,
};

#[derive(Debug)]
pub enum ParserError {
    ExpectPeek {
        token: Option<Token>,
        expected: TokenType,
    },
    TypeParse {
        token: Option<Token>,
        desired_type: String,
        literal: String,
    },
    EoF {
        token: Option<Token>,
        description: String,
    },
    InvalidPrefixFn {
        token: Option<Token>,
        name: String,
    },
    InvalidInfixFn {
        token: Option<Token>,
        name: String,
    },
}

pub struct Parser {
    tokens: TokenStream,
    errors: Vec<ParserError>,
}

impl Parser {
    pub fn new(tokens: TokenStream) -> Self {
        Parser {
            tokens,
            errors: Vec::new(),
        }
    }

    pub fn errors(&self) -> &Vec<ParserError> {
        &self.errors
    }

    pub fn expect_peek(&mut self, token_type: TokenType) -> bool {
        if self.tokens.peek_is(token_type) {
            self.tokens.next();
            true
        } else {
            self.errors.push(ParserError::ExpectPeek {
                token: self.tokens.peek().map_or(None, |t| Some(t.clone())),
                expected: token_type,
            });
            false
        }
    }

    pub fn expect_peek_and_consume(&mut self, token_type: TokenType) -> bool {
        if self.expect_peek(token_type) {
            self.tokens.next();
            true
        } else {
            false
        }
    }

    pub fn try_eat_semicolon(&mut self) {
        if self.tokens.peek_is(TokenType::Semicolon) {
            self.tokens.next();
        }
    }

    pub fn parse(&mut self) -> Program {
        let mut program = Program::new();
        loop {
            let next = self.tokens.current();
            match next {
                Some(tok) => {
                    if tok.token_type() == &TokenType::EndOfFile {
                        break;
                    }
                    let statement = self.parse_statement();
                    if let Some(statement) = statement {
                        program.add_statement(statement);
                    }
                    self.tokens.next();
                }
                None => break,
            }
        }
        program
    }

    fn parse_statement(&mut self) -> Option<Statement> {
        match self.tokens.current() {
            Some(token) => match token.token_type() {
                TokenType::Let => self.parse_let(),
                TokenType::Return => self.parse_return(),
                _ => self.parse_expression_statement(),
            },
            None => None,
        }
    }

    fn parse_let(&mut self) -> Option<Statement> {
        let tok = match self.tokens.current() {
            Some(tok) => tok.clone(),
            None => return None,
        };

        if !self.expect_peek(TokenType::Ident) {
            return None;
        }

        let name = match self.tokens.current() {
            None => return None,
            Some(tok) => tok,
        };

        let name = Identifier::new(name.literal().to_string());

        if !self.expect_peek_and_consume(TokenType::Assign) {
            return None;
        }

        let expression = self.parse_expression(Precedence::Bottom);

        self.try_eat_semicolon();

        Some(Statement::new(
            tok,
            StatementEnum::Let(Let::new(name, expression)),
        ))
    }

    fn parse_return(&mut self) -> Option<Statement> {
        let tok = match self.tokens.current() {
            Some(tok) => tok.clone(),
            None => return None,
        };

        if self.tokens.peek_is(TokenType::RBrace) {
            return Some(Statement::new(tok, StatementEnum::Return(None)));
        }
        // pop the current token
        self.tokens.next();

        if self.tokens.current_is(TokenType::Semicolon) {
            return Some(Statement::new(tok, StatementEnum::Return(None)));
        }

        let expression = self.parse_expression(Precedence::Bottom);

        self.try_eat_semicolon();

        return Some(Statement::new(tok, StatementEnum::Return(expression)));
    }

    fn parse_expression_statement(&mut self) -> Option<Statement> {
        let tok = match self.tokens.current() {
            Some(tok) => tok.clone(),
            None => return None,
        };
        let expression = self.parse_expression(Precedence::Bottom);

        self.try_eat_semicolon();

        return Some(Statement::new(tok, StatementEnum::Expression(expression)));
    }

    fn parse_expression(&mut self, precedence: Precedence) -> Option<Expression> {
        let tok = match self.tokens.current() {
            None => return None,
            Some(tok) => tok.clone(),
        };
        let mut prefix_fn = match self.parse_prefix_fn(tok) {
            None => return None,
            Some(prefix_fn) => prefix_fn,
        };

        loop {
            let peek_tok = match self.tokens.peek() {
                None => return None,
                Some(tok) => tok.clone(),
            };
            let peek_prec = Precedence::from_token(&peek_tok);
            if peek_tok.token_type() == &TokenType::Semicolon || precedence >= peek_prec {
                break;
            }
            if !self.is_infix_fn(&peek_tok) {
                return Some(prefix_fn);
            }
            self.tokens.next();
            prefix_fn = match self.parse_infix_fn(peek_tok, prefix_fn) {
                None => return None,
                Some(infix_fn) => infix_fn,
            }
        }
        Some(prefix_fn)
    }

    fn parse_block_statement(&mut self) -> Option<BlockStatement> {
        let tok = match self.tokens.current() {
            Some(tok) => tok.clone(),
            None => return None,
        };
        let mut statements = Vec::new();
        self.tokens.next();
        loop {
            if self.tokens.current_is(TokenType::RBrace) {
                break;
            }
            if self.tokens.current().is_none() {
                self.errors.push(ParserError::EoF {
                    description: "couldn't find matching } for block".to_string(),
                    token: Some(tok),
                });
                return None;
            }
            statements.push(self.parse_statement());
            self.tokens.next();
        }
        return Some(statements.into_iter().filter_map(|x| x).collect());
    }

    fn is_infix_fn(&mut self, tok: &Token) -> bool {
        match tok.token_type() {
            TokenType::Plus => true,
            TokenType::Minus => true,
            TokenType::Slash => true,
            TokenType::Asterisk => true,
            TokenType::EQ => true,
            TokenType::NE => true,
            TokenType::LT => true,
            TokenType::GT => true,
            TokenType::LParen => true,
            TokenType::LBracket => true,
            _ => false,
        }
    }

    fn parse_infix_fn(&mut self, tok: Token, left: Expression) -> Option<Expression> {
        match tok.token_type() {
            TokenType::Plus => self.parse_infix_expression(tok, left),
            TokenType::Minus => self.parse_infix_expression(tok, left),
            TokenType::Slash => self.parse_infix_expression(tok, left),
            TokenType::Asterisk => self.parse_infix_expression(tok, left),
            TokenType::EQ => self.parse_infix_expression(tok, left),
            TokenType::NE => self.parse_infix_expression(tok, left),
            TokenType::LT => self.parse_infix_expression(tok, left),
            TokenType::GT => self.parse_infix_expression(tok, left),
            TokenType::LParen => self.parse_call_expression(tok, left),
            TokenType::LBracket => self.parse_index_expression(tok, left),
            _ => {
                self.errors.push(ParserError::InvalidInfixFn {
                    name: tok.literal().to_string(),
                    token: Some(tok),
                });
                None
            }
        }
    }

    fn parse_parameters(&mut self, end_token: TokenType) -> Option<Vec<Expression>> {
        self.tokens.next();
        if self.tokens.current_is(end_token) {
            return Some(Vec::new());
        }
        let mut args = Vec::new();
        args.push(self.parse_expression(Precedence::Bottom));
        loop {
            if !self.tokens.peek_is(TokenType::Comma) {
                break;
            };
            self.tokens.next();
            self.tokens.next();
            args.push(self.parse_expression(Precedence::Bottom));
        }
        if !self.expect_peek(end_token) {
            return None;
        }
        return Some(args.into_iter().filter_map(|x| x).collect());
    }

    fn parse_call_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
        let params = match self.parse_parameters(TokenType::RParen) {
            None => return None,
            Some(params) => params,
        };

        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Call(Call::new(
                params.into_iter().map(|x| Box::new(x)).collect(),
                Box::new(left),
            )),
        ))
    }

    fn parse_index_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
        self.tokens.next();
        let index = match self.parse_expression(Precedence::Bottom) {
            None => return None,
            Some(index) => index,
        };

        if !self.expect_peek(TokenType::RBracket) {
            return None;
        }

        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Index(Index::new(Box::new(left), Box::new(index))),
        ))
    }

    fn parse_infix_expression(&mut self, tok: Token, left: Expression) -> Option<Expression> {
        let prec = Precedence::from_token(&tok);
        self.tokens.next();
        let right = match self.parse_expression(prec) {
            None => return None,
            Some(expr) => expr,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Infix(Infix::new(
                Box::new(left),
                Operator::parse(tok.literal().to_string()),
                Box::new(right),
            )),
        ))
    }

    fn parse_prefix_fn(&mut self, tok: Token) -> Option<Expression> {
        match tok.token_type() {
            TokenType::Ident => self.parse_identifier(tok),
            TokenType::Integer => self.parse_integer_literal(tok),
            TokenType::Bang => self.parse_prefix_expression(tok),
            TokenType::Minus => self.parse_prefix_expression(tok),
            TokenType::True => self.parse_boolean(tok),
            TokenType::False => self.parse_boolean(tok),
            TokenType::LParen => self.parse_grouped_expression(),
            TokenType::If => self.parse_if_expression(tok),
            TokenType::While => self.parse_while_expression(tok),
            TokenType::Function => self.parse_function_literal(tok),
            TokenType::String => self.parse_string(tok),
            TokenType::LBrace => self.parse_hash_literal(tok),
            TokenType::LBracket => self.parse_array_literal(tok),
            _ => {
                self.errors.push(ParserError::InvalidPrefixFn {
                    name: tok.literal().to_string(),
                    token: Some(tok),
                });
                None
            }
        }
    }

    pub fn parse_identifier(&mut self, tok: Token) -> Option<Expression> {
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Identifier(Identifier::new(tok.literal().to_string())),
        ))
    }

    pub fn parse_integer_literal(&mut self, tok: Token) -> Option<Expression> {
        let int_lit = match tok.literal().parse::<i64>() {
            Ok(n) => n,
            Err(e) => {
                self.errors.push(ParserError::TypeParse {
                    literal: tok.literal().to_string(),
                    desired_type: "i64".to_string(),
                    token: Some(tok),
                });
                return None;
            }
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::IntegerLiteral(ValueExpression::new(int_lit)),
        ))
    }

    pub fn parse_string(&mut self, tok: Token) -> Option<Expression> {
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::StringLiteral(ValueExpression::new(tok.literal().to_string())),
        ))
    }

    pub fn parse_boolean(&mut self, tok: Token) -> Option<Expression> {
        let bool_lit = match tok.literal().parse::<bool>() {
            Ok(n) => n,
            Err(e) => {
                self.errors.push(ParserError::TypeParse {
                    literal: tok.literal().to_string(),
                    desired_type: "bool".to_string(),
                    token: Some(tok),
                });
                return None;
            }
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Boolean(ValueExpression::new(bool_lit)),
        ))
    }

    pub fn parse_prefix_expression(&mut self, tok: Token) -> Option<Expression> {
        self.tokens.next();
        let right = match self.parse_expression(Precedence::Prefix) {
            None => return None,
            Some(exp) => exp,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::Prefix(Prefix::new(
                Operator::parse(tok.literal().to_string()),
                Box::new(right),
            )),
        ))
    }

    pub fn parse_if_expression(&mut self, tok: Token) -> Option<Expression> {
        if !self.expect_peek_and_consume(TokenType::LParen) {
            return None;
        }
        let condition = match self.parse_expression(Precedence::Bottom) {
            None => return None,
            Some(expr) => expr,
        };

        if !self.expect_peek(TokenType::RParen) || !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let true_branch = match self.parse_block_statement() {
            None => return None,
            Some(stmt) => stmt,
        };
        let false_branch = match self.tokens.peek_is(TokenType::Else) {
            true => {
                self.tokens.next();
                if !self.expect_peek(TokenType::LBrace) {
                    None
                } else {
                    self.parse_block_statement()
                }
            }
            false => None,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::If(If::new(Box::new(condition), true_branch, false_branch)),
        ))
    }
    pub fn parse_grouped_expression(&mut self) -> Option<Expression> {
        self.tokens.next();
        let expr = self.parse_expression(Precedence::Bottom);
        if !self.expect_peek(TokenType::RParen) {
            return None;
        }
        return expr;
    }
    pub fn parse_while_expression(&mut self, tok: Token) -> Option<Expression> {
        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = match self.parse_block_statement() {
            None => return None,
            Some(x) => x,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::While(body),
        ))
    }
    pub fn parse_function_literal(&mut self, tok: Token) -> Option<Expression> {
        if !self.expect_peek(TokenType::LParen) {
            return None;
        }

        let args = match self.parse_parameters(TokenType::RParen) {
            None => return None,
            Some(x) => x,
        };
        let mut identifiers = Vec::with_capacity(args.len());
        for arg in args.into_iter() {
            match arg.inner {
                ast::ExpressionEnum::Identifier(ident) => identifiers.push(ident),
                _ => return None,
            }
        }
        if !self.expect_peek(TokenType::LBrace) {
            return None;
        }

        let body = match self.parse_block_statement() {
            None => return None,
            Some(x) => x,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::FunctionLiteral(FunctionLiteral::new(identifiers, body)),
        ))
    }

    pub fn parse_array_literal(&mut self, tok: Token) -> Option<Expression> {
        let args = match self.parse_parameters(TokenType::RBracket) {
            None => return None,
            Some(x) => x,
        };
        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::ArrayLiteral(ArrayLiteral::new(
                args.into_iter().map(|x| Box::new(x)).collect(),
            )),
        ))
    }

    pub fn parse_hash_literal(&mut self, tok: Token) -> Option<Expression> {
        let mut pairs = Vec::new();

        loop {
            if self.tokens.peek_is(TokenType::RBrace) {
                break;
            }
            self.tokens.next();
            let key = match self.parse_expression(Precedence::Bottom) {
                None => return None,
                Some(x) => x,
            };
            if !self.expect_peek_and_consume(TokenType::Colon) {
                return None;
            }

            let value = match self.parse_expression(Precedence::Bottom) {
                None => return None,
                Some(x) => x,
            };

            pairs.push((key, value));

            if !self.tokens.peek_is(TokenType::RBrace) && !self.expect_peek(TokenType::Comma) {
                return None;
            }
        }
        if !self.expect_peek(TokenType::RBrace) {
            return None;
        }

        Some(Expression::new(
            tok.clone(),
            ast::ExpressionEnum::HashLiteral(HashLiteral::new(pairs)),
        ))
    }
}

#[derive(Eq, PartialEq)]
enum Precedence {
    Bottom,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
    Index,
}

impl Into<usize> for &Precedence {
    fn into(self) -> usize {
        match self {
            Precedence::Bottom => 1,
            Precedence::Equals => 2,
            Precedence::LessGreater => 3,
            Precedence::Sum => 4,
            Precedence::Product => 5,
            Precedence::Prefix => 6,
            Precedence::Call => 7,
            Precedence::Index => 8,
        }
    }
}

impl Ord for Precedence {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let x: usize = self.into();
        x.cmp(&other.into())
    }
}

impl PartialOrd for Precedence {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl Precedence {
    fn from_token(token: &Token) -> Precedence {
        match token.token_type() {
            TokenType::EQ => Precedence::Equals,
            TokenType::NE => Precedence::Equals,
            TokenType::Plus => Precedence::Sum,
            TokenType::Minus => Precedence::Sum,
            TokenType::Slash => Precedence::Product,
            TokenType::Asterisk => Precedence::Product,
            TokenType::LT => Precedence::LessGreater,
            TokenType::GT => Precedence::LessGreater,
            TokenType::LParen => Precedence::Call,
            TokenType::LBracket => Precedence::Index,
            _ => Precedence::Bottom,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn run_test(input: &str) -> Program {
        let tokens = TokenStream::from_string(input);
        let parser = &mut Parser::new(tokens);
        let program = parser.parse();
        program
    }

    #[test]
    fn test_statement_parsing() {
        let line = "
        let x = 5;
        let y = 10;
        let foobar = 10101;
        ";
        let expected = ["x", "y", "foobar"];

        let program = run_test(line);
        assert_eq!(program.statements.len(), 3);
        for (i, exp) in expected.iter().enumerate() {
            let let_stmnt = program.statements[i].get_let().unwrap();
            assert_eq!(let_stmnt.identifier.value, *exp);
        }
    }

    #[test]
    fn test_statement_with_expression() {
        let line = "let x = 5 + 50;\nlet y = true;";
        let program = run_test(line);
        let let_stmt = program.statements[0].get_let().unwrap();
        assert_eq!(let_stmt.identifier.value, "x");

        let infix = let_stmt.expr.as_ref().unwrap().get_infix().unwrap();
        assert_eq!(infix.op, Operator::parse("+".to_string()));
        assert_eq!(infix.left.get_integer().unwrap().value, 5);
        assert_eq!(infix.right.get_integer().unwrap().value, 50);

        let let_stmt = program.statements[1].get_let().unwrap();
        assert_eq!(let_stmt.identifier.value, "y");
        assert_eq!(
            let_stmt.expr.as_ref().unwrap().get_bool().unwrap().value,
            true
        );
    }

    #[test]
    fn test_return_statement_parsing() {
        let line = "
        return 5; 
        return 10;
        return 9999;
        ";
        let expected = [5, 10, 9999];
        let program = run_test(line);
        assert_eq!(program.statements.len(), 3);

        for (index, exp) in expected.iter().enumerate() {
            let ret_stmt = program.statements[index].get_return().unwrap();
            assert_eq!(ret_stmt.get_integer().unwrap().value, *exp);
        }
    }

    #[test]
    fn test_return_statement_expression() {
        let line = "
        return 6 + 7;
        return add(1, 2);
        ";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 2);

        let ret_stmt = program.statements[0].get_return().unwrap();
        let infix = ret_stmt.get_infix().unwrap();
        assert_eq!(infix.op, Operator::parse("+".to_string()));
        assert_eq!(infix.left.get_integer().unwrap().value, 6);
        assert_eq!(infix.right.get_integer().unwrap().value, 7);

        let ret_stmt = program.statements[1].get_return().unwrap();
        let call_expr = ret_stmt.get_call().unwrap();
        assert_eq!(call_expr.callable.get_identifier().unwrap().value, "add");
        assert_eq!(call_expr.args[0].get_integer().unwrap().value, 1);
        assert_eq!(call_expr.args[1].get_integer().unwrap().value, 2);
    }

    #[test]
    fn test_identifier_expression_statement() {
        let line = "foobar;";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let expr_statement = program.statements[0].get_expr().unwrap();
        assert_eq!(expr_statement.get_identifier().unwrap().value, "foobar");
    }

    #[test]
    fn test_integer_expression_statement() {
        let line = "5;";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let expr_statement = program.statements[0].get_expr().unwrap();
        assert_eq!(expr_statement.get_integer().unwrap().value, 5);
    }

    #[test]
    fn test_operator_parsing() {
        let line = "
        !5;
        -15;
        ";
        let expected = [("!", 5), ("-", 15)];
        let program = run_test(line);
        assert_eq!(program.statements.len(), expected.len());
        for (index, (op, val)) in expected.iter().enumerate() {
            let expr_stmnt = program.statements[index].get_expr().unwrap();
            let prefix = expr_stmnt.get_prefix().unwrap();
            assert_eq!(prefix.operator, Operator::parse(op.to_string()));
            assert_eq!(prefix.left.get_integer().unwrap().value, *val);
        }
    }

    #[test]
    fn test_hash_literal_parsing() {
        let line = r#"
        {"one": 1, "two": 2, "three": 3}
        "#;
        let expected = [("one", 1), ("two", 2), ("three", 3)];
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let hash_literal = program.statements[0]
            .get_expr()
            .unwrap()
            .get_hash()
            .unwrap();
        for (index, (exp_key, exp_val)) in expected.iter().enumerate() {
            let (key, value) = &hash_literal.pairs[index];
            assert_eq!(key.get_string().unwrap().value, *exp_key);
            assert_eq!(value.get_integer().unwrap().value, *exp_val);
        }
    }

    #[test]
    fn test_empty_hash_parsing() {
        let line = "{}";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let statement = program.statements[0].get_expr().unwrap();
        let hash_literal = statement.get_hash().unwrap();
        assert_eq!(hash_literal.pairs.len(), 0);
    }

    #[test]
    fn test_while_expression_parsing() {
        let line = "while { true; }";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);
        let while_stmt = program.statements[0]
            .get_expr()
            .unwrap()
            .get_while()
            .unwrap();

        assert_eq!(while_stmt.len(), 1);
        assert_eq!(
            while_stmt[0].get_expr().unwrap().get_bool().unwrap().value,
            true
        );
    }

    #[test]
    fn test_if_expression_parsing() {
        let line = "if (x < y) { x }";
        let program = run_test(line);

        assert_eq!(program.statements.len(), 1);
        let if_stmt = program.statements[0].get_expr().unwrap().get_if().unwrap();

        let condition = if_stmt.condition.get_infix().unwrap();
        assert_eq!(condition.left.get_identifier().unwrap().value, "x");
        assert_eq!(condition.right.get_identifier().unwrap().value, "y");
        assert_eq!(condition.op, Operator::parse("<".to_string()));

        assert_eq!(
            if_stmt.true_statement[0]
                .get_expr()
                .unwrap()
                .get_identifier()
                .unwrap()
                .value,
            "x"
        );
    }

    #[test]
    fn test_if_else_parsing() {
        let line = "if (x > y) { w } else { z; q }";
        let program = run_test(line);

        assert_eq!(program.statements.len(), 1);
        let if_expr = program.statements[0].get_expr().unwrap().get_if().unwrap();
        let condition = if_expr.condition.get_infix().unwrap();
        let left = if_expr.true_statement[0]
            .get_expr()
            .unwrap()
            .get_identifier()
            .unwrap();
        let right = if_expr.false_statement.as_ref().unwrap();

        assert_eq!(condition.op, Operator::parse(">".to_string()));
        assert_eq!(condition.left.get_identifier().unwrap().value, "x");
        assert_eq!(condition.right.get_identifier().unwrap().value, "y");

        assert_eq!(left.value, "w");

        assert_eq!(right.len(), 2);
        assert_eq!(
            right[0].get_expr().unwrap().get_identifier().unwrap().value,
            "z"
        );
        assert_eq!(
            right[1].get_expr().unwrap().get_identifier().unwrap().value,
            "q"
        );
    }

    #[test]
    fn test_function_literal_parsing() {
        let line = "fn(x, y) { x + y }";
        let program = run_test(line);

        assert_eq!(program.statements.len(), 1);

        let func_stmt = program.statements[0]
            .get_expr()
            .unwrap()
            .get_function_literal()
            .unwrap();
        assert_eq!(func_stmt.identifiers.len(), 2);

        assert_eq!(func_stmt.identifiers[0].value, "x");
        assert_eq!(func_stmt.identifiers[1].value, "y");

        assert_eq!(func_stmt.block.len(), 1);

        let infx = func_stmt.block[0].get_expr().unwrap().get_infix().unwrap();
        assert_eq!(infx.left.get_identifier().unwrap().value, "x");
        assert_eq!(infx.right.get_identifier().unwrap().value, "y");
        assert_eq!(infx.op, Operator::parse("+".to_string()));
    }

    #[test]
    fn test_call_expression_parsing() {
        let line = "add(1, 2 * 3, 4 + 5);";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let call_stmt = program.statements[0]
            .get_expr()
            .unwrap()
            .get_call()
            .unwrap();

        assert_eq!(call_stmt.args.len(), 3);
        let first = call_stmt.args[0].get_integer().unwrap();
        let second = call_stmt.args[1].get_infix().unwrap();
        let third = call_stmt.args[2].get_infix().unwrap();

        assert_eq!(first.value, 1);
        assert_eq!(second.op, Operator::parse("*".to_string()));
        assert_eq!(second.left.get_integer().unwrap().value, 2);
        assert_eq!(second.right.get_integer().unwrap().value, 3);

        assert_eq!(third.op, Operator::parse("+".to_string()));
        assert_eq!(third.left.get_integer().unwrap().value, 4);
        assert_eq!(third.right.get_integer().unwrap().value, 5);
    }

    #[test]
    fn test_sting_parsing() {
        let line = r#"let x = "test 123";"#;
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let let_stmt = program.statements[0].get_let().unwrap();
        assert_eq!(let_stmt.identifier.value, "x");
        assert_eq!(
            let_stmt.expr.as_ref().unwrap().get_string().unwrap().value,
            "test 123"
        );
    }

    #[test]
    fn test_array_parsing() {
        let line = "[3, 4, 5 + 6, fn(x) { x; }];";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let array_lit = program.statements[0]
            .get_expr()
            .unwrap()
            .get_array()
            .unwrap();
        assert_eq!(array_lit.elements.len(), 4);

        assert_eq!(array_lit.elements[0].get_integer().unwrap().value, 3);
        assert_eq!(array_lit.elements[1].get_integer().unwrap().value, 4);
        let infix = array_lit.elements[2].get_infix().unwrap();
        assert_eq!(infix.left.get_integer().unwrap().value, 5);
        assert_eq!(infix.right.get_integer().unwrap().value, 6);
        assert_eq!(infix.op, Operator::parse("+".to_string()));

        let fnc = array_lit.elements[3].get_function_literal().unwrap();
        assert_eq!(fnc.identifiers.len(), 1);
        assert_eq!(fnc.block.len(), 1);

        assert_eq!(fnc.identifiers[0].value, "x");
        assert_eq!(
            fnc.block[0]
                .get_expr()
                .unwrap()
                .get_identifier()
                .unwrap()
                .value,
            "x"
        );
    }

    #[test]
    fn test_index_expression_parsing() {
        let line = "someArray[3+1];";
        let program = run_test(line);
        assert_eq!(program.statements.len(), 1);

        let index_expr = program.statements[0]
            .get_expr()
            .unwrap()
            .get_index()
            .unwrap();

        let infx = index_expr.index.get_infix().unwrap();
        assert_eq!(infx.left.get_integer().unwrap().value, 3);
        assert_eq!(infx.right.get_integer().unwrap().value, 1);
        assert_eq!(infx.op, Operator::parse("+".to_string()));
        assert_eq!(
            index_expr.indexable.get_identifier().unwrap().value,
            "someArray"
        );
    }
}
