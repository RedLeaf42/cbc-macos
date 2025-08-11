package net.loveruby.cflat.ir;

public class DefaultIRVisitor<S, E> implements IRVisitor<S, E> {
    @Override
    public S visit(ExprStmt s) {
        s.expr().accept(this);
        return null;
    }

    @Override
    public S visit(Assign s) {
        s.rhs.accept(this);
        s.lhs.accept(this);
        return null;
    }

    @Override
    public S visit(CJump s) {
        return null;
    }

    @Override
    public S visit(Jump s) {
        return null;
    }

    @Override
    public S visit(Switch s) {
        return null;
    }

    @Override
    public S visit(LabelStmt s) {
        return null;
    }

    @Override
    public S visit(Return s) {
        return null;
    }

    @Override
    public E visit(Uni s) {
        s.expr().accept(this);
        return null;
    }

    @Override
    public E visit(Bin s) {
        return null;
    }

    @Override
    public E visit(Call s) {
        for (Expr e : s.args()) {
            e.accept(this);
        }
        return null;
    }

    @Override
    public E visit(Addr s) {
        return null;
    }

    @Override
    public E visit(Mem s) {
        s.expr().accept(this);
        return null;
    }

    @Override
    public E visit(Var s) {
        return null;
    }

    @Override
    public E visit(Int s) {
        return null;
    }

    @Override
    public E visit(Float s) {
        return null;
    }

    @Override
    public E visit(Str s) {
        return null;
    }

    @Override
    public E visit(Cast s) {
        s.expr().accept(this);
        return null;
    }
}
