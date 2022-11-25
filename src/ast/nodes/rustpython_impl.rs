
use std::iter::{Cloned, Map};
use std::slice::Iter;

use num_bigint::BigInt as RspyBigInt;
use rustpython_parser::ast as rspy_ast;

use super::{
    Alias, AnnAssign, Arg, Arguments, Assert, Assign, Ast, AsyncFor, AsyncFunctionDef, AsyncWith,
    Attribute, AugAssign, Await, BigInt, BinOp, BoolOp, Boolop, Call, ClassDef, Cmpop, Compare,
    Comprehension, Constant, ConstantExpr, ConstantKind, Delete, Dict, DictComp, ExceptHandler,
    Expr, ExprContext, ExprKind, For, FormattedValue, FunctionDef, GeneratorExp, Global, Ident, If,
    IfExp, Import, ImportFrom, JoinedStr, Keyword, Lambda, List, ListComp, Located, Match, MatchAs,
    MatchCase, MatchClass, MatchMapping, MatchOr, MatchSequence, MatchSingleton, MatchStar,
    MatchValue, Name, NamedExpr, Nonlocal, Operator, Pattern, PatternKind, Raise, Return, Set,
    SetComp, Slice, Starred, Stmt, StmtKind, Subscript, Try, Tuple, TypeComment, UnaryOp, Unaryop,
    While, With, Withitem, Yield, YieldFrom,
};

impl<T, U> Located for rspy_ast::Located<T, U> {
    #[inline]
    fn start_row(&self) -> usize {
        self.location.row()
    }

    #[inline]
    fn start_col(&self) -> usize {
        self.location.column()
    }

    #[inline]
    fn end_row(&self) -> usize {
        self.end_location.as_ref().unwrap().row()
    }

    #[inline]
    fn end_col(&self) -> usize {
        self.end_location.as_ref().unwrap().column()
    }
}

impl Ident for String {
    #[inline]
    fn val(&self) -> &str {
        self.as_str()
    }
}

impl TypeComment for String {
    #[inline]
    fn val(&self) -> &str {
        self.as_str()
    }
}

impl<U> Alias for rspy_ast::Alias<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn name(&self) -> <Self::Ast as Ast>::Ident<'_> {
        &self.node.name
    }

    #[inline]
    fn asname(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        self.node.asname.as_ref()
    }
}

impl<U> Arg for rspy_ast::Arg<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn arg(&self) -> <Self::Ast as Ast>::Ident<'_> {
        &self.node.arg
    }

    #[inline]
    fn annotation(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        self.node.annotation.as_deref()
    }

    #[inline]
    fn type_comment(&self) -> Option<<Self::Ast as Ast>::TypeComment<'_>> {
        self.node.type_comment.as_ref()
    }
}

impl<U> Arguments for rspy_ast::Arguments<U> {
    type ArgsIter<'a> = Iter<'a, rspy_ast::Arg<U>>
            where U: 'a;
    type Ast = RspyAst<U>;
    type DefaultsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
            where U: 'a;
    type KwDefaultsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
            where U: 'a;
    type KwonlyargsIter<'a> = Iter<'a, rspy_ast::Arg<U>>
            where U: 'a;
    type PosonlyargsIter<'a> = Iter<'a, rspy_ast::Arg<U>>
            where U: 'a;

    #[inline]
    fn posonlyargs(&self) -> Self::PosonlyargsIter<'_> {
        self.posonlyargs.iter()
    }

    #[inline]
    fn args(&self) -> Self::ArgsIter<'_> {
        self.args.iter()
    }

    #[inline]
    fn vararg(&self) -> Option<<Self::Ast as Ast>::Arg<'_>> {
        self.vararg.as_deref()
    }

    #[inline]
    fn kwonlyargs(&self) -> Self::KwonlyargsIter<'_> {
        self.kwonlyargs.iter()
    }

    #[inline]
    fn kw_defaults(&self) -> Self::KwDefaultsIter<'_> {
        self.kw_defaults.iter()
    }

    #[inline]
    fn kwarg(&self) -> Option<<Self::Ast as Ast>::Arg<'_>> {
        self.kwarg.as_deref()
    }

    #[inline]
    fn defaults(&self) -> Self::DefaultsIter<'_> {
        self.defaults.iter()
    }
}
impl<U> Keyword for rspy_ast::Keyword<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn arg(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        self.node.arg.as_ref()
    }

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        &self.node.value
    }
}

impl BigInt for RspyBigInt {}

// TODO(Seamooo) potentially remove clones and copies below?

impl Constant for rspy_ast::Constant {
    type BigInt<'a> = &'a RspyBigInt;
    type BytesIter<'a> = Cloned<Iter<'a, u8>>;
    type Constant<'a> = &'a rspy_ast::Constant;
    type TupleIter<'a> = Iter<'a, rspy_ast::Constant>;

    fn value(
        &self,
    ) -> ConstantKind<&str, Self::BytesIter<'_>, Self::TupleIter<'_>, Self::BigInt<'_>> {
        match self {
            Self::None => ConstantKind::None,
            Self::Bool(x) => ConstantKind::Bool(*x),
            Self::Str(x) => ConstantKind::Str(x.as_str()),
            Self::Bytes(x) => ConstantKind::Bytes(x.iter().cloned()),
            Self::Int(x) => ConstantKind::Int(x),
            Self::Tuple(x) => ConstantKind::Tuple(x.iter()),
            Self::Float(x) => ConstantKind::Float(*x),
            Self::Complex { real, imag } => ConstantKind::Complex {
                real: *real,
                imag: *imag,
            },
            Self::Ellipsis => ConstantKind::Ellipsis,
        }
    }
}

impl<U> Comprehension for rspy_ast::Comprehension<U> {
    type Ast = RspyAst<U>;
    type IfsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where
        U: 'a;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        &self.target
    }

    #[inline]
    fn iter(&self) -> <Self::Ast as Ast>::Expr<'_> {
        &self.iter
    }

    #[inline]
    fn ifs(&self) -> Self::IfsIter<'_> {
        self.ifs.iter()
    }

    #[inline]
    fn is_async(&self) -> usize {
        self.is_async
    }
}

impl From<&rspy_ast::Boolop> for Boolop {
    fn from(val: &rspy_ast::Boolop) -> Self {
        match val {
            rspy_ast::Boolop::And => Self::And,
            rspy_ast::Boolop::Or => Self::Or,
        }
    }
}

impl<U> BoolOp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type ValuesIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn op(&self) -> Boolop {
        match self {
            Self::BoolOp { op, .. } => op.into(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn values(&self) -> Self::ValuesIter<'_> {
        match self {
            Self::BoolOp { values, .. } => values.iter(),
            _ => unreachable!(),
        }
    }
}

impl<U> NamedExpr for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::NamedExpr { target, .. } => target,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::NamedExpr { value, .. } => value,
            _ => unreachable!(),
        }
    }
}

impl From<&rspy_ast::Operator> for Operator {
    fn from(val: &rspy_ast::Operator) -> Self {
        match val {
            rspy_ast::Operator::Add => Self::Add,
            rspy_ast::Operator::Sub => Self::Sub,
            rspy_ast::Operator::Mult => Self::Mult,
            rspy_ast::Operator::MatMult => Self::MatMult,
            rspy_ast::Operator::Div => Self::Div,
            rspy_ast::Operator::Mod => Self::Mod,
            rspy_ast::Operator::Pow => Self::Pow,
            rspy_ast::Operator::LShift => Self::LShift,
            rspy_ast::Operator::RShift => Self::RShift,
            rspy_ast::Operator::BitOr => Self::BitOr,
            rspy_ast::Operator::BitXor => Self::BitXor,
            rspy_ast::Operator::BitAnd => Self::BitAnd,
            rspy_ast::Operator::FloorDiv => Self::FloorDiv,
        }
    }
}

impl<U> BinOp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn left(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::BinOp { left, .. } => left,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn op(&self) -> Operator {
        match self {
            Self::BinOp { op, .. } => op.into(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn right(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::BinOp { right, .. } => right,
            _ => unreachable!(),
        }
    }
}

impl From<&rspy_ast::Unaryop> for Unaryop {
    fn from(val: &rspy_ast::Unaryop) -> Self {
        match val {
            rspy_ast::Unaryop::Invert => Self::Invert,
            rspy_ast::Unaryop::Not => Self::Not,
            rspy_ast::Unaryop::UAdd => Self::UAdd,
            rspy_ast::Unaryop::USub => Self::USub,
        }
    }
}

impl<U> UnaryOp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn op(&self) -> Unaryop {
        match self {
            Self::UnaryOp { op, .. } => op.into(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn operand(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::UnaryOp { operand, .. } => operand,
            _ => unreachable!(),
        }
    }
}
impl<U> Lambda for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn args(&self) -> <Self::Ast as Ast>::Arguments<'_> {
        match self {
            Self::Lambda { args, .. } => args,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Lambda { body, .. } => body,
            _ => unreachable!(),
        }
    }
}
impl<U> IfExp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn test(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::IfExp { test, .. } => test,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::IfExp { body, .. } => body,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::IfExp { orelse, .. } => orelse,
            _ => unreachable!(),
        }
    }
}
impl<U> Dict for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type KeysIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;
    type ValuesIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn keys(&self) -> Self::KeysIter<'_> {
        match self {
            Self::Dict { keys, .. } => keys.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn values(&self) -> Self::ValuesIter<'_> {
        match self {
            Self::Dict { values, .. } => values.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Set for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type EltsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn elts(&self) -> Self::EltsIter<'_> {
        match self {
            Self::Set { elts } => elts.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> ListComp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type GeneratorsIter<'a> = Iter<'a, rspy_ast::Comprehension<U>>
    where U: 'a;

    #[inline]
    fn elt(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::ListComp { elt, .. } => elt,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn generators(&self) -> Self::GeneratorsIter<'_> {
        match self {
            Self::ListComp { generators, .. } => generators.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> SetComp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type GeneratorsIter<'a> = Iter<'a, rspy_ast::Comprehension<U>>
    where U: 'a;

    #[inline]
    fn elt(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::SetComp { elt, .. } => elt,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn generators(&self) -> Self::GeneratorsIter<'_> {
        match self {
            Self::SetComp { generators, .. } => generators.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> DictComp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type GeneratorsIter<'a> = Iter<'a, rspy_ast::Comprehension<U>>
        where U: 'a;

    #[inline]
    fn key(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::DictComp { key, .. } => key,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::DictComp { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn generators(&self) -> Self::GeneratorsIter<'_> {
        match self {
            Self::DictComp { generators, .. } => generators.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> GeneratorExp for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type GeneratorsIter<'a> = Iter<'a, rspy_ast::Comprehension<U>>
    where U: 'a;

    #[inline]
    fn elt(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::GeneratorExp { elt, .. } => elt,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn generators(&self) -> Self::GeneratorsIter<'_> {
        match self {
            Self::GeneratorExp { generators, .. } => generators.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Await for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Await { value } => value,
            _ => unreachable!(),
        }
    }
}
impl<U> Yield for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Yield { value } => value.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> YieldFrom for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::YieldFrom { value } => value,
            _ => unreachable!(),
        }
    }
}
impl<'a> From<&'a rspy_ast::Cmpop> for Cmpop {
    fn from(val: &'a rspy_ast::Cmpop) -> Self {
        match val {
            rspy_ast::Cmpop::Eq => Self::Eq,
            rspy_ast::Cmpop::NotEq => Self::NotEq,
            rspy_ast::Cmpop::Lt => Self::Lt,
            rspy_ast::Cmpop::LtE => Self::LtE,
            rspy_ast::Cmpop::Gt => Self::Gt,
            rspy_ast::Cmpop::GtE => Self::GtE,
            rspy_ast::Cmpop::Is => Self::Is,
            rspy_ast::Cmpop::IsNot => Self::IsNot,
            rspy_ast::Cmpop::In => Self::In,
            rspy_ast::Cmpop::NotIn => Self::NotIn,
        }
    }
}
impl<U> Compare for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type CmpopIter<'a> =
            Map<Iter<'a, rspy_ast::Cmpop>, fn(&'a rspy_ast::Cmpop) -> Cmpop>
        where U: 'a;

    #[inline]
    fn left(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Compare { left, .. } => left,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ops(&self) -> Self::CmpopIter<'_> {
        match self {
            Self::Compare { ops, .. } => ops.iter().map(Cmpop::from),
            _ => unreachable!(),
        }
    }
}
impl<U> Call for rspy_ast::ExprKind<U> {
    type ArgsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;
    type Ast = RspyAst<U>;
    type KeywordsIter<'a> = Iter<'a, rspy_ast::Keyword<U>>
    where U: 'a;

    #[inline]
    fn func(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Call { func, .. } => func,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn args(&self) -> Self::ArgsIter<'_> {
        match self {
            Self::Call { args, .. } => args.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn keywords(&self) -> Self::KeywordsIter<'_> {
        match self {
            Self::Call { keywords, .. } => keywords.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> FormattedValue for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::FormattedValue { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn conversion(&self) -> usize {
        match self {
            Self::FormattedValue { conversion, .. } => *conversion,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn format_spec(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::FormattedValue { format_spec, .. } => format_spec.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> JoinedStr for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type ValuesIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn values(&self) -> Self::ValuesIter<'_> {
        match self {
            Self::JoinedStr { values } => values.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> ConstantExpr for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Constant<'_> {
        match self {
            Self::Constant { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn kind(&self) -> Option<&str> {
        match self {
            Self::Constant { kind, .. } => kind.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl From<&rspy_ast::ExprContext> for ExprContext {
    fn from(val: &rspy_ast::ExprContext) -> Self {
        match val {
            rspy_ast::ExprContext::Load => Self::Load,
            rspy_ast::ExprContext::Store => Self::Store,
            rspy_ast::ExprContext::Del => Self::Del,
        }
    }
}
impl<U> Attribute for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Attribute { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn attr(&self) -> <Self::Ast as Ast>::Ident<'_> {
        match self {
            Self::Attribute { attr, .. } => attr,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::Attribute { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> Subscript for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Subscript { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn slice(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Subscript { slice, .. } => slice,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::Subscript { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> Starred for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Starred { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::Starred { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> Name for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn id(&self) -> <Self::Ast as Ast>::Ident<'_> {
        match self {
            Self::Name { id, .. } => id,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::Name { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> List for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type EltsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn elts(&self) -> Self::EltsIter<'_> {
        match self {
            Self::List { elts, .. } => elts.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::List { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> Tuple for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;
    type EltsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn elts(&self) -> Self::EltsIter<'_> {
        match self {
            Self::Tuple { elts, .. } => elts.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn ctx(&self) -> ExprContext {
        match self {
            Self::Tuple { ctx, .. } => ctx.into(),
            _ => unreachable!(),
        }
    }
}
impl<U> Slice for rspy_ast::ExprKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn lower(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Slice { lower, .. } => lower.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn upper(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Slice { upper, .. } => upper.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn step(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Slice { step, .. } => step.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> Expr for rspy_ast::Expr<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn expr(
        &self,
    ) -> ExprKind<
        <Self::Ast as Ast>::BoolOp<'_>,
        <Self::Ast as Ast>::NamedExpr<'_>,
        <Self::Ast as Ast>::BinOp<'_>,
        <Self::Ast as Ast>::UnaryOp<'_>,
        <Self::Ast as Ast>::Lambda<'_>,
        <Self::Ast as Ast>::IfExp<'_>,
        <Self::Ast as Ast>::Dict<'_>,
        <Self::Ast as Ast>::Set<'_>,
        <Self::Ast as Ast>::ListComp<'_>,
        <Self::Ast as Ast>::SetComp<'_>,
        <Self::Ast as Ast>::DictComp<'_>,
        <Self::Ast as Ast>::GeneratorExp<'_>,
        <Self::Ast as Ast>::Await<'_>,
        <Self::Ast as Ast>::Yield<'_>,
        <Self::Ast as Ast>::YieldFrom<'_>,
        <Self::Ast as Ast>::Compare<'_>,
        <Self::Ast as Ast>::Call<'_>,
        <Self::Ast as Ast>::FormattedValue<'_>,
        <Self::Ast as Ast>::JoinedStr<'_>,
        <Self::Ast as Ast>::ConstantExpr<'_>,
        <Self::Ast as Ast>::Attribute<'_>,
        <Self::Ast as Ast>::Subscript<'_>,
        <Self::Ast as Ast>::Starred<'_>,
        <Self::Ast as Ast>::Name<'_>,
        <Self::Ast as Ast>::List<'_>,
        <Self::Ast as Ast>::Tuple<'_>,
        <Self::Ast as Ast>::Slice<'_>,
    > {
        match &self.node {
            rspy_ast::ExprKind::BoolOp { .. } => ExprKind::BoolOp(&self.node),
            rspy_ast::ExprKind::NamedExpr { .. } => ExprKind::NamedExpr(&self.node),
            rspy_ast::ExprKind::BinOp { .. } => ExprKind::BinOp(&self.node),
            rspy_ast::ExprKind::UnaryOp { .. } => ExprKind::UnaryOp(&self.node),
            rspy_ast::ExprKind::Lambda { .. } => ExprKind::Lambda(&self.node),
            rspy_ast::ExprKind::IfExp { .. } => ExprKind::IfExp(&self.node),
            rspy_ast::ExprKind::Dict { .. } => ExprKind::Dict(&self.node),
            rspy_ast::ExprKind::Set { .. } => ExprKind::Set(&self.node),
            rspy_ast::ExprKind::ListComp { .. } => ExprKind::ListComp(&self.node),
            rspy_ast::ExprKind::SetComp { .. } => ExprKind::SetComp(&self.node),
            rspy_ast::ExprKind::DictComp { .. } => ExprKind::DictComp(&self.node),
            rspy_ast::ExprKind::GeneratorExp { .. } => ExprKind::GeneratorExp(&self.node),
            rspy_ast::ExprKind::Await { .. } => ExprKind::Await(&self.node),
            rspy_ast::ExprKind::Yield { .. } => ExprKind::Yield(&self.node),
            rspy_ast::ExprKind::YieldFrom { .. } => ExprKind::YieldFrom(&self.node),
            rspy_ast::ExprKind::Compare { .. } => ExprKind::Compare(&self.node),
            rspy_ast::ExprKind::Call { .. } => ExprKind::Call(&self.node),
            rspy_ast::ExprKind::FormattedValue { .. } => ExprKind::FormattedValue(&self.node),
            rspy_ast::ExprKind::JoinedStr { .. } => ExprKind::JoinedStr(&self.node),
            rspy_ast::ExprKind::Constant { .. } => ExprKind::ConstantExpr(&self.node),
            rspy_ast::ExprKind::Attribute { .. } => ExprKind::Attribute(&self.node),
            rspy_ast::ExprKind::Subscript { .. } => ExprKind::Subscript(&self.node),
            rspy_ast::ExprKind::Starred { .. } => ExprKind::Starred(&self.node),
            rspy_ast::ExprKind::Name { .. } => ExprKind::Name(&self.node),
            rspy_ast::ExprKind::List { .. } => ExprKind::List(&self.node),
            rspy_ast::ExprKind::Tuple { .. } => ExprKind::Tuple(&self.node),
            rspy_ast::ExprKind::Slice { .. } => ExprKind::Slice(&self.node),
        }
    }
}

impl<U> ExceptHandler for rspy_ast::Excepthandler<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn type_(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match &self.node {
            rspy_ast::ExcepthandlerKind::ExceptHandler { type_, .. } => type_.as_deref(),
        }
    }

    #[inline]
    fn name(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        match &self.node {
            rspy_ast::ExcepthandlerKind::ExceptHandler { name, .. } => name.as_ref(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match &self.node {
            rspy_ast::ExcepthandlerKind::ExceptHandler { body, .. } => body.iter(),
        }
    }
}
impl<U> MatchValue for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::MatchValue { value } => value,
            _ => unreachable!(),
        }
    }
}
impl<U> MatchSingleton for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Constant<'_> {
        match self {
            Self::MatchSingleton { value } => value,
            _ => unreachable!(),
        }
    }
}
impl<U> MatchSequence for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;
    type PatternsIter<'a> = Iter<'a, rspy_ast::Pattern<U>>
        where U: 'a;

    #[inline]
    fn patterns(&self) -> Self::PatternsIter<'_> {
        match self {
            Self::MatchSequence { patterns } => patterns.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> MatchMapping for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;
    type KeysIter<'a> = Iter<'a, rspy_ast::Expr<U>>
        where U: 'a;
    type PatternsIter<'a> = Iter<'a, rspy_ast::Pattern<U>>
    where U: 'a;

    #[inline]
    fn keys(&self) -> Self::KeysIter<'_> {
        match self {
            Self::MatchMapping { keys, .. } => keys.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn patterns(&self) -> Self::PatternsIter<'_> {
        match self {
            Self::MatchMapping { patterns, .. } => patterns.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn rest(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        match self {
            Self::MatchMapping { rest, .. } => rest.as_ref(),
            _ => unreachable!(),
        }
    }
}
impl<U> MatchClass for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;
    type KwdAttrsIter<'a> = Iter<'a, String>
        where U: 'a;
    type KwdPatternsIter<'a> = Iter<'a, rspy_ast::Pattern<U>>
        where U: 'a;
    type PatternsIter<'a> = Iter<'a, rspy_ast::Pattern<U>>
        where U: 'a;

    #[inline]
    fn cls(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::MatchClass { cls, .. } => cls,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn patterns(&self) -> Self::PatternsIter<'_> {
        match self {
            Self::MatchClass { patterns, .. } => patterns.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn kwd_attrs(&self) -> Self::KwdAttrsIter<'_> {
        match self {
            Self::MatchClass { kwd_attrs, .. } => kwd_attrs.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn kwd_patterns(&self) -> Self::KwdPatternsIter<'_> {
        match self {
            Self::MatchClass { kwd_patterns, .. } => kwd_patterns.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> MatchStar for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn name(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        match self {
            Self::MatchStar { name } => name.as_ref(),
            _ => unreachable!(),
        }
    }
}
impl<U> MatchAs for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn pattern(&self) -> Option<<Self::Ast as Ast>::Pattern<'_>> {
        match self {
            Self::MatchAs { pattern, .. } => pattern.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn name(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        match self {
            Self::MatchAs { name, .. } => name.as_ref(),
            _ => unreachable!(),
        }
    }
}
impl<U> MatchOr for rspy_ast::PatternKind<U> {
    type Ast = RspyAst<U>;
    type PatternsIter<'a> = Iter<'a, rspy_ast::Pattern<U>>
    where U: 'a;

    #[inline]
    fn patterns(&self) -> Self::PatternsIter<'_> {
        match self {
            Self::MatchOr { patterns } => patterns.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Pattern for rspy_ast::Pattern<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn pattern(
        &self,
    ) -> PatternKind<
        <Self::Ast as Ast>::MatchValue<'_>,
        <Self::Ast as Ast>::MatchSingleton<'_>,
        <Self::Ast as Ast>::MatchSequence<'_>,
        <Self::Ast as Ast>::MatchMapping<'_>,
        <Self::Ast as Ast>::MatchClass<'_>,
        <Self::Ast as Ast>::MatchStar<'_>,
        <Self::Ast as Ast>::MatchAs<'_>,
        <Self::Ast as Ast>::MatchOr<'_>,
    > {
        match &self.node {
            rspy_ast::PatternKind::MatchValue { .. } => PatternKind::MatchValue(&self.node),
            rspy_ast::PatternKind::MatchSingleton { .. } => PatternKind::MatchSingleton(&self.node),
            rspy_ast::PatternKind::MatchSequence { .. } => PatternKind::MatchSequence(&self.node),
            rspy_ast::PatternKind::MatchMapping { .. } => PatternKind::MatchMapping(&self.node),
            rspy_ast::PatternKind::MatchClass { .. } => PatternKind::MatchClass(&self.node),
            rspy_ast::PatternKind::MatchStar { .. } => PatternKind::MatchStar(&self.node),
            rspy_ast::PatternKind::MatchAs { .. } => PatternKind::MatchAs(&self.node),
            rspy_ast::PatternKind::MatchOr { .. } => PatternKind::MatchOr(&self.node),
        }
    }
}
impl<U> Withitem for rspy_ast::Withitem<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn context_expr(&self) -> <Self::Ast as Ast>::Expr<'_> {
        &self.context_expr
    }

    #[inline]
    fn optional_vars(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        self.optional_vars.as_deref()
    }
}
impl<U> MatchCase for rspy_ast::MatchCase<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn pattern(&self) -> <Self::Ast as Ast>::Pattern<'_> {
        &self.pattern
    }

    #[inline]
    fn guard(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        self.guard.as_deref()
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        self.body.iter()
    }
}

impl<U> FunctionDef for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type DecoratorListIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn name(&self) -> <Self::Ast as Ast>::Ident<'_> {
        match self {
            Self::FunctionDef { name, .. } => name,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn args(&self) -> <Self::Ast as Ast>::Arguments<'_> {
        match self {
            Self::FunctionDef { args, .. } => args,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::FunctionDef { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn decorator_list(&self) -> Self::DecoratorListIter<'_> {
        match self {
            Self::FunctionDef { decorator_list, .. } => decorator_list.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn returns(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::FunctionDef { returns, .. } => returns.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::FunctionDef { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> AsyncFunctionDef for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type DecoratorListIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn name(&self) -> <Self::Ast as Ast>::Ident<'_> {
        match self {
            Self::AsyncFunctionDef { name, .. } => name,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn args(&self) -> <Self::Ast as Ast>::Arguments<'_> {
        match self {
            Self::AsyncFunctionDef { args, .. } => args,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::AsyncFunctionDef { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn decorator_list(&self) -> Self::DecoratorListIter<'_> {
        match self {
            Self::AsyncFunctionDef { decorator_list, .. } => decorator_list.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn returns(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::AsyncFunctionDef { returns, .. } => returns.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::AsyncFunctionDef { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> ClassDef for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BasesIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type DecoratorListIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;
    type KeywordsIter<'a> = Iter<'a, rspy_ast::Keyword<U>>
    where U: 'a;

    #[inline]
    fn name(&self) -> <Self::Ast as Ast>::Ident<'_> {
        match self {
            Self::ClassDef { name, .. } => name,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn bases(&self) -> Self::BasesIter<'_> {
        match self {
            Self::ClassDef { bases, .. } => bases.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn keywords(&self) -> Self::KeywordsIter<'_> {
        match self {
            Self::ClassDef { keywords, .. } => keywords.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::ClassDef { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn decorator_list(&self) -> Self::DecoratorListIter<'_> {
        match self {
            Self::ClassDef { decorator_list, .. } => decorator_list.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Return for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn value(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Return { value } => value.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> Delete for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type TargetsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn targets(&self) -> Self::TargetsIter<'_> {
        match self {
            Self::Delete { targets } => targets.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Assign for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type TargetsIter<'a> = Iter<'a, rspy_ast::Expr<U>>
    where U: 'a;

    #[inline]
    fn targets(&self) -> Self::TargetsIter<'_> {
        match self {
            Self::Assign { targets, .. } => targets.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Assign { value, .. } => value,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::Assign { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> AugAssign for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AugAssign { target, .. } => target,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn op(&self) -> Operator {
        match self {
            Self::AugAssign { op, .. } => op.into(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn value(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AugAssign { value, .. } => value,
            _ => unreachable!(),
        }
    }
}
impl<U> AnnAssign for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AnnAssign { target, .. } => target,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn annotation(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AnnAssign { annotation, .. } => annotation,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn value(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::AnnAssign { value, .. } => value.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn simple(&self) -> usize {
        match self {
            Self::AnnAssign { simple, .. } => *simple,
            _ => unreachable!(),
        }
    }
}
impl<U> For for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type OrelseIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::For { target, .. } => target,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn iter(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::For { iter, .. } => iter,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::For { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> Self::OrelseIter<'_> {
        match self {
            Self::For { orelse, .. } => orelse.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::For { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> AsyncFor for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type OrelseIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn target(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AsyncFor { target, .. } => target,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn iter(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::AsyncFor { iter, .. } => iter,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::AsyncFor { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> Self::OrelseIter<'_> {
        match self {
            Self::AsyncFor { orelse, .. } => orelse.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::AsyncFor { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> While for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type OrelseIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn test(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::While { test, .. } => test,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::While { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> Self::OrelseIter<'_> {
        match self {
            Self::While { orelse, .. } => orelse.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> If for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type OrelseIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn test(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::If { test, .. } => test,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::If { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> Self::OrelseIter<'_> {
        match self {
            Self::If { orelse, .. } => orelse.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> With for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type ItemsIter<'a> = Iter<'a, rspy_ast::Withitem<U>>
    where U: 'a;

    #[inline]
    fn items(&self) -> Self::ItemsIter<'_> {
        match self {
            Self::With { items, .. } => items.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::With { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::With { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> AsyncWith for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type ItemsIter<'a> = Iter<'a, rspy_ast::Withitem<U>>
    where U: 'a;

    #[inline]
    fn items(&self) -> Self::ItemsIter<'_> {
        match self {
            Self::AsyncWith { items, .. } => items.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::AsyncWith { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn type_comment(&self) -> Option<&str> {
        match self {
            Self::AsyncWith { type_comment, .. } => type_comment.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> Match for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type CasesIter<'a> = Iter<'a, rspy_ast::MatchCase<U>>
    where U: 'a;

    #[inline]
    fn subject(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Match { subject, .. } => subject,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn cases(&self) -> Self::CasesIter<'_> {
        match self {
            Self::Match { cases, .. } => cases.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Raise for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn exc(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Raise { exc, .. } => exc.as_deref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn cause(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Raise { cause, .. } => cause.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> Try for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type BodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type FinalbodyIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;
    type HandlersIter<'a> = Iter<'a, rspy_ast::Excepthandler<U>>
    where U: 'a;
    type OrelseIter<'a> = Iter<'a, rspy_ast::Stmt<U>>
    where U: 'a;

    #[inline]
    fn body(&self) -> Self::BodyIter<'_> {
        match self {
            Self::Try { body, .. } => body.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn handlers(&self) -> Self::HandlersIter<'_> {
        match self {
            Self::Try { handlers, .. } => handlers.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn orelse(&self) -> Self::OrelseIter<'_> {
        match self {
            Self::Try { orelse, .. } => orelse.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn finalbody(&self) -> Self::FinalbodyIter<'_> {
        match self {
            Self::Try { finalbody, .. } => finalbody.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Assert for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;

    #[inline]
    fn test(&self) -> <Self::Ast as Ast>::Expr<'_> {
        match self {
            Self::Assert { test, .. } => test,
            _ => unreachable!(),
        }
    }

    #[inline]
    fn msg(&self) -> Option<<Self::Ast as Ast>::Expr<'_>> {
        match self {
            Self::Assert { msg, .. } => msg.as_deref(),
            _ => unreachable!(),
        }
    }
}
impl<U> Import for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type NamesIter<'a> = Iter<'a, rspy_ast::Alias<U>>
    where U: 'a;

    #[inline]
    fn names(&self) -> Self::NamesIter<'_> {
        match self {
            Self::Import { names } => names.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> ImportFrom for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type NamesIter<'a> = Iter<'a, rspy_ast::Alias<U>>
    where U: 'a;

    #[inline]
    fn module(&self) -> Option<<Self::Ast as Ast>::Ident<'_>> {
        match self {
            Self::ImportFrom { module, .. } => module.as_ref(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn names(&self) -> Self::NamesIter<'_> {
        match self {
            Self::ImportFrom { names, .. } => names.iter(),
            _ => unreachable!(),
        }
    }

    #[inline]
    fn level(&self) -> Option<usize> {
        match self {
            Self::ImportFrom { level, .. } => *level,
            _ => unreachable!(),
        }
    }
}
impl<U> Global for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type NamesIter<'a> = Iter<'a, String>
        where U: 'a;

    #[inline]
    fn names(&self) -> Self::NamesIter<'_> {
        match self {
            Self::Global { names } => names.iter(),
            _ => unreachable!(),
        }
    }
}
impl<U> Nonlocal for rspy_ast::StmtKind<U> {
    type Ast = RspyAst<U>;
    type NamesIter<'a> = Iter<'a, String>
        where U: 'a;

    #[inline]
    fn names(&self) -> Self::NamesIter<'_> {
        match self {
            Self::Nonlocal { names } => names.iter(),
            _ => unreachable!(),
        }
    }
}

impl<U> Stmt for rspy_ast::Stmt<U> {
    type Ast = RspyAst<U>;

    fn stmt(
        &self,
    ) -> StmtKind<
        <Self::Ast as Ast>::FunctionDef<'_>,
        <Self::Ast as Ast>::AsyncFunctionDef<'_>,
        <Self::Ast as Ast>::ClassDef<'_>,
        <Self::Ast as Ast>::Return<'_>,
        <Self::Ast as Ast>::Delete<'_>,
        <Self::Ast as Ast>::Assign<'_>,
        <Self::Ast as Ast>::AugAssign<'_>,
        <Self::Ast as Ast>::AnnAssign<'_>,
        <Self::Ast as Ast>::For<'_>,
        <Self::Ast as Ast>::AsyncFor<'_>,
        <Self::Ast as Ast>::While<'_>,
        <Self::Ast as Ast>::If<'_>,
        <Self::Ast as Ast>::With<'_>,
        <Self::Ast as Ast>::AsyncWith<'_>,
        <Self::Ast as Ast>::Match<'_>,
        <Self::Ast as Ast>::Raise<'_>,
        <Self::Ast as Ast>::Try<'_>,
        <Self::Ast as Ast>::Assert<'_>,
        <Self::Ast as Ast>::Import<'_>,
        <Self::Ast as Ast>::ImportFrom<'_>,
        <Self::Ast as Ast>::Global<'_>,
        <Self::Ast as Ast>::Nonlocal<'_>,
        <Self::Ast as Ast>::Expr<'_>,
    > {
        match &self.node {
            rspy_ast::StmtKind::FunctionDef { .. } => StmtKind::FunctionDef(&self.node),
            rspy_ast::StmtKind::AsyncFunctionDef { .. } => StmtKind::AsyncFunctionDef(&self.node),
            rspy_ast::StmtKind::ClassDef { .. } => StmtKind::ClassDef(&self.node),
            rspy_ast::StmtKind::Return { .. } => StmtKind::Return(&self.node),
            rspy_ast::StmtKind::Delete { .. } => StmtKind::Delete(&self.node),
            rspy_ast::StmtKind::Assign { .. } => StmtKind::Assign(&self.node),
            rspy_ast::StmtKind::AugAssign { .. } => StmtKind::AugAssign(&self.node),
            rspy_ast::StmtKind::AnnAssign { .. } => StmtKind::AnnAssign(&self.node),
            rspy_ast::StmtKind::For { .. } => StmtKind::For(&self.node),
            rspy_ast::StmtKind::AsyncFor { .. } => StmtKind::AsyncFor(&self.node),
            rspy_ast::StmtKind::While { .. } => StmtKind::While(&self.node),
            rspy_ast::StmtKind::If { .. } => StmtKind::If(&self.node),
            rspy_ast::StmtKind::With { .. } => StmtKind::With(&self.node),
            rspy_ast::StmtKind::AsyncWith { .. } => StmtKind::AsyncWith(&self.node),
            rspy_ast::StmtKind::Match { .. } => StmtKind::Match(&self.node),
            rspy_ast::StmtKind::Raise { .. } => StmtKind::Raise(&self.node),
            rspy_ast::StmtKind::Try { .. } => StmtKind::Try(&self.node),
            rspy_ast::StmtKind::Assert { .. } => StmtKind::Assert(&self.node),
            rspy_ast::StmtKind::Import { .. } => StmtKind::Import(&self.node),
            rspy_ast::StmtKind::ImportFrom { .. } => StmtKind::ImportFrom(&self.node),
            rspy_ast::StmtKind::Global { .. } => StmtKind::Global(&self.node),
            rspy_ast::StmtKind::Nonlocal { .. } => StmtKind::Nonlocal(&self.node),
            rspy_ast::StmtKind::Expr { value } => StmtKind::Expr(value),
            rspy_ast::StmtKind::Pass => StmtKind::Pass,
            rspy_ast::StmtKind::Break => StmtKind::Break,
            rspy_ast::StmtKind::Continue => StmtKind::Continue,
        }
    }
}

pub struct RspyAst<U = ()>(U);

impl<U> Ast for RspyAst<U> {
    type Alias<'a> = &'a rspy_ast::Alias<U>
        where U: 'a;
    type AnnAssign<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Arg<'a> = &'a rspy_ast::Arg<U>
        where U: 'a;
    type Arguments<'a> = &'a rspy_ast::Arguments<U>
        where U: 'a;
    type Assert<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Assign<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type AsyncFor<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type AsyncFunctionDef<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type AsyncWith<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Attribute<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type AugAssign<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Await<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type BigInt<'a> = &'a RspyBigInt
        where U: 'a;
    type BinOp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type BoolOp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Call<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type ClassDef<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Compare<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Comprehension<'a> = &'a rspy_ast::Comprehension<U>
        where U: 'a;
    type Constant<'a> = &'a rspy_ast::Constant
        where U: 'a;
    type ConstantExpr<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Delete<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Dict<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type DictComp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type ExceptHandler<'a> = &'a rspy_ast::Excepthandler<U>
        where U: 'a;
    type Expr<'a> = &'a rspy_ast::Expr<U>
        where U: 'a;
    type For<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type FormattedValue<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type FunctionDef<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type GeneratorExp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Global<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Ident<'a> = &'a String
        where U: 'a;
    type If<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type IfExp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Import<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type ImportFrom<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type JoinedStr<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Keyword<'a> = &'a rspy_ast::Keyword<U>
        where U: 'a;
    type Lambda<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type List<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type ListComp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Match<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type MatchAs<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchCase<'a> = &'a rspy_ast::MatchCase<U>
        where U: 'a;
    type MatchClass<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchMapping<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchOr<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchSequence<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchSingleton<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchStar<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type MatchValue<'a> = &'a rspy_ast::PatternKind<U>
        where U: 'a;
    type Name<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type NamedExpr<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Nonlocal<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Pattern<'a> = &'a rspy_ast::Pattern<U>
        where U: 'a;
    type Raise<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Return<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Set<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type SetComp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Slice<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Starred<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Stmt<'a> = &'a rspy_ast::Stmt<U>
        where U: 'a;
    type Subscript<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type Try<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Tuple<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type TypeComment<'a> = &'a String
        where U: 'a;
    type UnaryOp<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type While<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type With<'a> = &'a rspy_ast::StmtKind<U>
        where U: 'a;
    type Withitem<'a> = &'a rspy_ast::Withitem<U>
        where U: 'a;
    type Yield<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
    type YieldFrom<'a> = &'a rspy_ast::ExprKind<U>
        where U: 'a;
}