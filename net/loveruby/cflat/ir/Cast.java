package net.loveruby.cflat.ir;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Cast extends Expr {
    protected Expr expr;
    protected net.loveruby.cflat.type.Type sourceType;
    protected net.loveruby.cflat.type.Type targetType;

    public Cast(Type type, Expr expr, net.loveruby.cflat.type.Type sourceType,
            net.loveruby.cflat.type.Type targetType) {
        super(type);
        this.expr = expr;
        this.sourceType = sourceType;
        this.targetType = targetType;
    }

    public Expr expr() {
        return expr;
    }

    public net.loveruby.cflat.type.Type sourceType() {
        return sourceType;
    }

    public net.loveruby.cflat.type.Type targetType() {
        return targetType;
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
        d.printMember("expr", expr);
        d.printMember("sourceType", sourceType);
        d.printMember("targetType", targetType);
    }
}
