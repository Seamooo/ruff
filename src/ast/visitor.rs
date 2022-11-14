use super::nodes::{
    Arguments, Ast, AsyncFunctionDef, Boolop, ClassDef, Cmpop, Comprehension, ConstantKind,
    ExceptHandler, Expr, ExprContext, ExprKind, FunctionDef, MatchCase, Operator, PatternKind,
    Stmt, StmtKind, Unaryop,
};

// use crate::ast::helpers::match_name_or_attr;

/// Return `true` if the `Expr` is a name or attribute reference to `${target}`.
pub fn match_name_or_attr<'a, E: Expr<'a>>(expr: &E, target: &str) -> bool {
    match &expr.expr() {
        ExprKind::Attribute(attribute) => target == attribute.attr(),
        ExprKind::Name(name) => target == name.id(),
        _ => false,
    }
}

pub trait Visitor<'a, T: Ast<'a>> {
    fn visit_stmt(&mut self, stmt: &'a T::Stmt) {
        walk_stmt(self, stmt);
    }
    fn visit_annotation(&mut self, expr: &'a T::Expr) {
        walk_expr(self, expr);
    }
    fn visit_expr(&mut self, expr: &'a T::Expr) {
        walk_expr(self, expr);
    }
    fn visit_constant(&mut self, constant: &'a T::Constant) {
        walk_constant(self, constant);
    }
    fn visit_expr_context(&mut self, expr_content: &'a ExprContext) {
        walk_expr_context(self, expr_content);
    }
    fn visit_boolop(&mut self, boolop: &'a Boolop) {
        walk_boolop(self, boolop);
    }
    fn visit_operator(&mut self, operator: &'a Operator) {
        walk_operator(self, operator);
    }
    fn visit_unaryop(&mut self, unaryop: &'a Unaryop) {
        walk_unaryop(self, unaryop);
    }
    fn visit_cmpop(&mut self, cmpop: &'a Cmpop) {
        walk_cmpop(self, cmpop);
    }
    fn visit_comprehension(&mut self, comprehension: &'a T::Comprehension) {
        walk_comprehension(self, comprehension);
    }
    fn visit_excepthandler(&mut self, excepthandler: &'a T::ExceptHandler) {
        walk_excepthandler(self, excepthandler);
    }
    fn visit_arguments(&mut self, arguments: &'a T::Arguments) {
        walk_arguments(self, arguments);
    }
    fn visit_arg(&mut self, arg: &'a T::Arg) {
        walk_arg(self, arg);
    }
    fn visit_keyword(&mut self, keyword: &'a T::Keyword) {
        walk_keyword(self, keyword);
    }
    fn visit_alias(&mut self, alias: &'a T::Alias) {
        walk_alias(self, alias);
    }
    fn visit_withitem(&mut self, withitem: &'a T::Withitem) {
        walk_withitem(self, withitem);
    }
    fn visit_match_case(&mut self, match_case: &'a T::MatchCase) {
        walk_match_case(self, match_case);
    }
    fn visit_pattern(&mut self, pattern: &'a T::Pattern) {
        walk_pattern(self, pattern);
    }
}

pub fn walk_stmt<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(visitor: &mut V, stmt: &'a T::Stmt) {
    match &stmt.stmt() {
        StmtKind::FunctionDef(function_def) => {
            visitor.visit_arguments(function_def.args());
            for expr in function_def.decorator_list().iter() {
                visitor.visit_expr(expr);
            }
            for expr in function_def.returns().iter() {
                visitor.visit_expr(expr);
            }
            for stmt in function_def.body().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::AsyncFunctionDef(function_def) => {
            visitor.visit_arguments(function_def.args());
            for expr in function_def.decorator_list().iter() {
                visitor.visit_expr(expr);
            }
            for expr in function_def.returns().iter() {
                visitor.visit_expr(expr);
            }
            for stmt in function_def.body().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::ClassDef(class_def) => {
            for expr in class_def.bases().iter() {
                visitor.visit_expr(expr);
            }
            for keyword in class_def.keywords().iter() {
                visitor.visit_keyword(keyword);
            }
            for expr in class_def.decorator_list().iter() {
                visitor.visit_expr(expr);
            }
            for stmt in class_def.body().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::Return(return_) => {
            if let Some(expr) = return_.value() {
                visitor.visit_expr(expr);
            }
        }
        StmtKind::Delete(delete) => {
            for expr in delete.targets().iter() {
                visitor.visit_expr(expr);
            }
        }
        StmtKind::Assign(assign) => {
            visitor.visit_expr(assign.value());
            for expr in assign.targets().iter() {
                visitor.visit_expr(expr);
            }
        }
        StmtKind::AugAssign(aug_assign) => {
            visitor.visit_expr(aug_assign.target());
            visitor.visit_operator(aug_assign.op());
            visitor.visit_expr(aug_assign.value());
        }
        StmtKind::AnnAssign(ann_assign) => {
            visitor.visit_annotation(ann_assign.annotation());
            if let Some(expr) = ann_assign.value() {
                if match_name_or_attr(ann_assign.annotation(), "TypeAlias") {
                    visitor.visit_annotation(expr);
                } else {
                    visitor.visit_expr(expr);
                }
            }
            visitor.visit_expr(ann_assign.target());
        }
        StmtKind::For(for_) => {
            visitor.visit_expr(for_.target());
            visitor.visit_expr(for_.iter());
            for stmt in for_.body().iter() {
                visitor.visit_stmt(stmt);
            }
            for stmt in for_.orelse().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::AsyncFor(async_for) => {
            visitor.visit_expr(async_for.target());
            visitor.visit_expr(async_for.iter());
            for stmt in async_for.body().iter() {
                visitor.visit_stmt(stmt);
            }
            for stmt in async_for.orelse().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::While(while_) => {
            visitor.visit_expr(while_.test());
            for stmt in while_.body().iter() {
                visitor.visit_stmt(stmt);
            }
            for stmt in while_.orelse().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::If(if_) => {
            visitor.visit_expr(if_.test());
            for stmt in if_.body().iter() {
                visitor.visit_stmt(stmt);
            }
            for stmt in if_.orelse().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::With(with) => {
            for withitem in with.items().iter() {
                visitor.visit_withitem(withitem);
            }
            for stmt in with.body().iter() {
                stmt.vsisit_stmt(stmt);
            }
        }
        StmtKind::AsyncWith(async_with) => {
            for withitem in async_with.items().iter() {
                visitor.visit_withitem(withitem);
            }
            for stmt in async_with.body().iter() {
                stmt.vsisit_stmt(stmt);
            }
        }
        StmtKind::Match(match_) => {
            // TODO(charlie): Handle `cases`.
            visitor.visit_expr(match_.subject());
            for match_case in match_.cases().iter() {
                visitor.visit_match_case(match_case);
            }
        }
        StmtKind::Raise(raise) => {
            if let Some(expr) = raise.exc() {
                visitor.visit_expr(expr);
            }
            if let Some(expr) = raise.cause() {
                visitor.visit_expr(expr);
            }
        }
        StmtKind::Try(try_) => {
            for stmt in try_.body().iter() {
                visitor.visit_stmt(stmt);
            }
            for excepthandler in try_.handlers().iter() {
                visitor.visit_excepthandler(excepthandler);
            }
            for stmt in try_.orelse().iter() {
                visitor.visit_stmt(stmt);
            }
            for stmt in try_.finalbody().iter() {
                visitor.visit_stmt(stmt);
            }
        }
        StmtKind::Assert(assert) => {
            for alias in assert.names().iter() {
                visitor.visit_alias(alias);
            }
        }
        StmtKind::Import(import) => {
            for alias in import.names().iter() {
                visitor.visit_alias(alias);
            }
        }
        StmtKind::ImportFrom(import_from) => {
            for alias in import_from.names().iter() {
                visitor.visit_alias(alias);
            }
        }
        StmtKind::Global(_) => {}
        StmtKind::Nonlocal(_) => {}
        StmtKind::Expr(expr) => visitor.visit_expr(expr),
        StmtKind::Pass => {}
        StmtKind::Break => {}
        StmtKind::Continue => {}
    };
}

pub fn walk_expr<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(visitor: &mut V, expr: &'a T::Expr) {
    match &expr.expr() {
        ExprKind::BoolOp(bool_op) => {
            visitor.visit_boolop(bool_op.op());
            for expr in bool_op.values().iter() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::NamedExpr(named_expr) => {
            visitor.visit_expr(named_expr.target());
            visitor.visit_expr(named_expr.value());
        }
        ExprKind::BinOp(bin_op) => {
            visitor.visit_expr(bin_op.left());
            visitor.visit_operator(bin_op.op());
            visitor.visit_expr(bin_op.right());
        }
        ExprKind::UnaryOp(unary_op) => {
            visitor.visit_unaryop(unary_op.op());
            visitor.visit_expr(unary_op.operand());
        }
        ExprKind::Lambda(lambda) => {
            visitor.visit_arguments(lambda.args());
            visitor.visit_arguments(lambda.args());
        }
        ExprKind::IfExp(if_exp) => {
            visitor.visit_expr(if_exp.test());
            visitor.visit_expr(if_exp.body());
            visitor.visit_expr(if_exp.orelse());
        }
        ExprKind::Dict(dict) => {
            for expr in dict.keys().iter() {
                visitor.visit_expr(expr);
            }
            for expr in dict.values().iter() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Set(set) => {
            for expr in set.elts().iter() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::ListComp(list_comp) => {
            for comprehension in list_comp.generators().iter() {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(list_comp.elt());
        }
        ExprKind::SetComp(set_comp) => {
            for comprehension in set_comp.generators().iter() {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(set_comp.elt());
        }
        ExprKind::DictComp(dict_comp) => {
            for comprehension in dict_comp.generators().iter() {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(dict_comp.key());
            visitor.visit_expr(dict_comp.value());
        }
        ExprKind::GeneratorExp(generator_exp) => {
            for comprehension in generator_exp.generators().iter() {
                visitor.visit_comprehension(comprehension);
            }
            visitor.visit_expr(generator_exp.elt());
        }
        ExprKind::Await(await_) => visitor.visit_expr(await_.value()),
        ExprKind::Yield(yield_) => {
            if let Some(expr) = yield_.value() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Compare(compare) => {
            visitor.visit_expr(compare.left());
            for cmpop in compare.ops().iter() {
                visitor.visit_cmpop(cmpop);
            }
            for expr in compare.comparators().iter() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Call(call) => {
            visitor.visit_expr(call.func());
            for expr in call.args().iter() {
                visitor.visit_expr(expr);
            }
            for keyword in call.keywords().iter() {
                visitor.visit_keyword(keyword);
            }
        }
        ExprKind::FormattedValue(formatted_value) => {
            visitor.visit_expr(formatted_value.value());
            if let Some(expr) = formatted_value.format_spec() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::JoinedStr(joined_str) => {
            for expr in joined_str.values().iter() {
                visitor.visit_expr(expr);
            }
        }
        ExprKind::Constant(constant) => visitor.visit_constant(constant.value()),
        ExprKind::Attribute(attribute) => {
            visitor.visit_expr(attribute.value());
            visitor.visit_expr_context(attribute.ctx());
        }
        ExprKind::Starred(starred) => {
            visitor.visit_expr(starred.value());
            visitor.visit_expr(starred.value());
        }
        ExprKind::Name(name) => {
            visitor.visit_expr_context(name.ctx());
        }
        ExprKind::List(list) => {
            for expr in list.elts().iter() {
                visitor.visit_expr(expr);
            }
            visitor.visit_expr_context(list.ctx());
        }
        ExprKind::Tuple(tuple) => {
            for expr in tuple.elts().iter() {
                visitor.visit_expr(expr);
            }
            visitor.visit_expr_context(tuple.ctx());
        }
        ExprKind::Slice(slice) => {
            if let Some(expr) = slice.lower() {
                visitor.visit_expr(expr);
            }
            if let Some(expr) = slice.upper() {
                visitor.visit_expr(expr);
            }
            if let Some(expr) = slice.step() {
                visitor.visit_expr(expr);
            }
        }
    }
}

pub fn walk_constant<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    constant: &'a T::Constant,
) {
    if let ConstantKind::Tuple(constants) = constant.value() {
        for constant in constants.iter() {
            visitor.visit_constant(constant);
        }
    }
}

pub fn walk_comprehension<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    comprehension: &'a T::Comprehension,
) {
    visitor.visit_expr(comprehension.target());
    visitor.visit_expr(comprehension.iter());
    for expr in comprehension.ifs().iter() {
        visitor.visit_expr(expr);
    }
}

pub fn walk_excepthandler<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    excepthandler: &'a T::ExceptHandler,
) {
    if let Some(expr) = excepthandler.type_() {
        visitor.visit_expr(expr);
    }
    for stmt in excepthandler.body().iter() {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_arguments<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    arguments: &'a T::Arguments,
) {
    for arg in arguments.posonlyargs.iter() {
        visitor.visit_arg(arg);
    }
    for arg in arguments.args.iter() {
        visitor.visit_arg(arg);
    }
    if let Some(arg) = arguments.vararg() {
        visitor.visit_arg(arg);
    }
    for arg in arguments.kwonlyargs.iter() {
        visitor.visit_arg(arg);
    }
    for expr in arguments.kw_defaults.iter() {
        visitor.visit_expr(expr);
    }
    if let Some(arg) = arguments.kwarg() {
        visitor.visit_arg(arg);
    }
    for expr in arguments.defaults() {
        visitor.visit_expr(expr);
    }
}

pub fn walk_arg<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(visitor: &mut V, arg: &'a T::Arg) {
    if let Some(expr) = arg.annotation() {
        visitor.visit_annotation(expr);
    }
}

pub fn walk_keyword<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    keyword: &'a T::Keyword,
) {
    visitor.visit_expr(keyword.value());
}

pub fn walk_withitem<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    withitem: &'a T::Withitem,
) {
    if let Some(expr) = withitem.optional_vars() {
        visitor.visit_expr(expr);
    }
}

pub fn walk_match_case<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    match_case: &'a T::MatchCase,
) {
    visitor.visit_pattern(match_case.pattern());
    if let Some(expr) = match_case.guard() {
        visitor.visit_expr(expr);
    }
    for stmt in match_case.body().iter() {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_pattern<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    pattern: &'a T::Pattern,
) {
    match pattern.value() {
        PatternKind::MatchValue(match_value) => visitor.visit_expr(match_value.value()),
        PatternKind::MatchSingleton(match_singleton) => {
            visitor.visit_constant(match_singleton.value())
        }
        PatternKind::MatchSequence(match_sequence) => {
            for pattern in match_sequence.patterns().iter() {
                visitor.visit_pattern(pattern);
            }
        }
        PatternKind::MatchMapping(match_mapping) => {
            for expr in match_mapping.keys().iter() {
                visitor.visit_expr(expr);
            }
            for pattern in match_mapping.patterns().iter() {
                visitor.visit_pattern(pattern);
            }
        }
        PatternKind::MatchClass(match_class) => {
            visitor.visit_expr(match_class.cls());
            for pattern in match_class.patterns().iter() {
                visitor.visit_pattern(pattern);
            }
            for pattern in match_class.kwd_patterns().iter() {
                visitor.visit_pattern(pattern);
            }
        }
        PatternKind::MatchStar(_) => {}
        PatternKind::MatchAs(match_as) => {
            if let Some(pattern) = match_as.pattern() {
                visitor.visit_pattern(pattern);
            }
        }
        PatternKind::MatchOr(match_or) => {
            for pattern in match_or.patterns().iter() {
                visitor.visit_pattern(pattern);
            }
        }
    }
}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_expr_context<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    expr_context: &'a ExprContext,
) {
}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_boolop<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    boolop: &'a Boolop,
) {
}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_operator<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    operator: &'a Operator,
) {
}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_unaryop<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    unaryop: &'a Unaryop,
) {
}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_cmpop<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(visitor: &mut V, cmpop: &'a Cmpop) {}

#[allow(unused_variables)]
#[inline(always)]
pub fn walk_alias<'a, T: Ast<'a>, V: Visitor<'a, T> + ?Sized>(
    visitor: &mut V,
    alias: &'a T::Alias,
) {
}
