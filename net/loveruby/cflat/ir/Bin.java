package net.loveruby.cflat.ir;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Bin extends Expr {
    protected Op op;
    protected Expr left, right;

    public Bin(Type type, Op op, Expr left, Expr right) {
        super(type);
        this.op = op;
        this.left = left;
        this.right = right;
    }

    public Expr left() {
        return left;
    }

    public Expr right() {
        return right;
    }

    public Op op() {
        return op;
    }

    public <S, E> E accept(IRVisitor<S, E> visitor) {
        return visitor.visit(this);
    }

    /**
     * 接受寄存器感知访问者
     * 
     * @param visitor        寄存器感知访问者
     * @param targetRegister 目标寄存器
     * @return 访问结果
     */
    public <S, E> E accept(RegisterAwareVisitor<S, E> visitor, Register targetRegister) {
        return visitor.visit(this, targetRegister);
    }

    protected void _dump(Dumper d) {
        d.printMember("op", op.toString());
        d.printMember("left", left);
        d.printMember("right", right);
    }
}
