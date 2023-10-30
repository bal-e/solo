use egg::{CostFunction, Extractor, RecExpr, Rewrite, Runner, rewrite};

use super::*;

/// An optimizer.
pub struct Optimizer {
    /// The optimizer's rules.
    rules: Vec<Rewrite<ExprNode, ()>>,
}

impl Optimizer {
    /// Optimize a [`RecExpr`].
    pub fn optimize(&mut self, expr: &mut RecExpr<ExprNode>) {
        // Fill the equality-saturation graph.
        let runner = Runner::default()
            .with_expr(expr)
            .run(&self.rules);

        // Extract the fastest derivation of the expression.
        *expr = Extractor::new(&runner.egraph, TimeCost)
            .find_best(runner.roots[0])
            .1;
    }
}

impl Default for Optimizer {
    fn default() -> Self {
        Self {
            rules: self::rules(),
        }
    }
}

/// A cost function based on expected computation time.
struct TimeCost;

impl CostFunction<ExprNode> for TimeCost {
    type Cost = usize;

    fn cost<C>(&mut self, node: &ExprNode, mut recurse: C) -> Self::Cost
    where C: FnMut(Id) -> Self::Cost {
        let op_cost = match node {
            ExprNode::Not(..) => 1,
            ExprNode::Add(..) | ExprNode::Sub(..) => 1,
            ExprNode::Mul(..) => 3,
            ExprNode::Div(..) | ExprNode::Rem(..) => 15,

            ExprNode::And(..) | ExprNode::IOr(..) | ExprNode::XOr(..) => 1,
            ExprNode::ShL(..) | ExprNode::ShR(..) => 3,

            ExprNode::Cat(..) => 0,
            ExprNode::Ind(..) => 1,
            ExprNode::Exp(..) => 1,
            ExprNode::Red(..) => 1,

            ExprNode::IsEq(..) | ExprNode::IsNE(..)
                | ExprNode::IsLT(..) | ExprNode::IsLE(..)
                | ExprNode::IsGT(..) | ExprNode::IsGE(..) => 3,

            ExprNode::Cond(..) => 1,
            ExprNode::Else(..) => 1,

            ExprNode::Int(..) => 1,
            ExprNode::Arg(..) => 0,
        };

        node.children().iter()
            .copied()
            .fold(op_cost, |c, i| c + recurse(i))
    }
}

/// Construct rewrite rules for the HIR.
fn rules() -> Vec<Rewrite<ExprNode, ()>> {
    vec![
        // Commutativity
        rewrite!("commute-add"; "(add ?a ?b)" => "(add ?b ?a)"),
        rewrite!("commute-mul"; "(mul ?a ?b)" => "(mul ?b ?a)"),
        rewrite!("commute-and"; "(and ?a ?b)" => "(and ?b ?a)"),
        rewrite!("commute-ior"; "(ior ?a ?b)" => "(ior ?b ?a)"),
        rewrite!("commute-xor"; "(xor ?a ?b)" => "(xor ?b ?a)"),
        rewrite!("commute-iseq"; "(iseq ?a ?b)" => "(iseq ?b ?a)"),
        rewrite!("commute-isne"; "(isne ?a ?b)" => "(isne ?b ?a)"),
        rewrite!("commute-islt"; "(islt ?a ?b)" => "(isgt ?b ?a)"),
        rewrite!("commute-isle"; "(isle ?a ?b)" => "(isge ?b ?a)"),
        rewrite!("commute-isgt"; "(isgt ?a ?b)" => "(islt ?b ?a)"),
        rewrite!("commute-isge"; "(isge ?a ?b)" => "(isle ?b ?a)"),

        // Associativity
        rewrite!("associate-add"; "(add ?a (add ?b ?c))" => "(add (add ?a ?b) ?c)"),
        rewrite!("associate-mul"; "(mul ?a (mul ?b ?c))" => "(mul (mul ?a ?b) ?c)"),
        rewrite!("associate-and"; "(and ?a (and ?b ?c))" => "(and (and ?a ?b) ?c)"),
        rewrite!("associate-ior"; "(ior ?a (ior ?b ?c))" => "(ior (ior ?a ?b) ?c)"),
        rewrite!("associate-xor"; "(xor ?a (xor ?b ?c))" => "(xor (xor ?a ?b) ?c)"),

        // Reflexivity
        rewrite!("reflex-and"; "(and ?a ?a)" => "?a"),
        rewrite!("reflex-ior"; "(ior ?a ?a)" => "?a"),
        rewrite!("reflex-xor"; "(xor ?a ?a)" => "(0)"),

        // Distributivity
        rewrite!("distribute-add-over-mul"; "(mul ?a (add ?b ?c))" => "(add (mul ?a ?b) (mul ?a ?c))"),
        rewrite!("collect-add-under-mul"; "(add (mul ?a ?b) (mul ?a ?c))" => "(mul ?a (add ?b ?c))"),
    ]
}
