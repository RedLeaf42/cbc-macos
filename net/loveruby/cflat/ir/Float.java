package net.loveruby.cflat.ir;

import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Float extends Expr {
    protected double value;
    protected net.loveruby.cflat.type.Type astType; // 保存原始的AST类型信息

    public Float(Type type, double value) {
        super(type);
        this.value = value;
        this.astType = null; // 默认为null
    }

    public Float(Type type, double value, net.loveruby.cflat.type.Type astType) {
        super(type);
        this.value = value;
        this.astType = astType;
    }

    public double value() {
        return value;
    }

    public boolean isConstant() {
        return true;
    }

    public net.loveruby.cflat.type.Type astType() {
        return astType;
    }

    public ImmediateValue asmValue() {
        // 对于浮点数，我们需要特殊处理
        // 这里暂时返回一个占位符，实际实现需要根据目标架构处理
        return new ImmediateValue(new IntegerLiteral((long) value));
    }

    public MemoryReference memref() {
        throw new Error("must not happen: FloatValue#memref");
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
        d.printMember("value", String.valueOf(value));
    }
}
