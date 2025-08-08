package net.loveruby.cflat.ir;
import net.loveruby.cflat.asm.*;
import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Int extends Expr {
    protected long value;

    public Int(Type type, long value) {
        super(type);
        this.value = value;
    }

    public long value() { return value; }

    public boolean isConstant() { return true; }

    public ImmediateValue asmValue() {
        return new ImmediateValue(new IntegerLiteral(value));
    }

    public MemoryReference memref() {
        throw new Error("must not happen: IntValue#memref");
    }

    public <S,E> E accept(IRVisitor<S,E> visitor) {
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
        d.printMember("value", value);
    }
}
