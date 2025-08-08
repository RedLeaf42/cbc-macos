package net.loveruby.cflat.ir;

import net.loveruby.cflat.asm.Type;
import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Mem extends Expr {
    protected Expr expr;

    public Mem(Type type, Expr expr) {
        super(type);
        this.expr = expr;
    }

    public Expr expr() {
        return expr;
    }

    public Expr addressNode(Type type) {
        return expr;
    }

    public <S, E> E accept(IRVisitor<S, E> visitor) {
        return visitor.visit(this);
    }

    /**
     * 接受寄存器感知访问者
     * @param visitor 寄存器感知访问者
     * @param targetRegister 目标寄存器
     * @return 访问结果
     */
    public <S,E> E accept(RegisterAwareVisitor<S,E> visitor, Register targetRegister) {
        return visitor.visit(this, targetRegister);
    }

    protected void _dump(Dumper d) {
        d.printMember("expr", expr);
    }
}
