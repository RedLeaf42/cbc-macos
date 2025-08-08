package net.loveruby.cflat.ir;
import net.loveruby.cflat.entity.ConstantEntry;
import net.loveruby.cflat.asm.Type;
import net.loveruby.cflat.asm.Operand;
import net.loveruby.cflat.asm.ImmediateValue;
import net.loveruby.cflat.asm.MemoryReference;
import net.loveruby.cflat.asm.Symbol;
import net.loveruby.cflat.sysdep.arm64.Register;

public class Str extends Expr {
    protected ConstantEntry entry;

    public Str(Type type, ConstantEntry entry) {
        super(type);
        this.entry = entry;
    }

    public ConstantEntry entry() { return entry; }

    public Symbol symbol() {
        return entry.symbol();
    }

    public boolean isConstant() { return true; }

    public MemoryReference memref() {
        return entry.memref();
    }

    public Operand address() {
        return entry.address();
    }

    public ImmediateValue asmValue() {
        return entry.address();
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
        d.printMember("entry", entry.toString());
    }
}
