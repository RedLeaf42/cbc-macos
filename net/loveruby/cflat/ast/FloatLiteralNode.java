package net.loveruby.cflat.ast;

import net.loveruby.cflat.type.TypeRef;

public class FloatLiteralNode extends LiteralNode {
    private double value;

    public FloatLiteralNode(Location loc, TypeRef ref, double value) {
        super(loc, ref);
        this.value = value;
    }

    public double value() {
        return value;
    }

    protected void _dump(Dumper d) {
        d.printMember("typeNode", typeNode);
        d.printMember("value", String.valueOf(value));
    }

    public <S, E> E accept(ASTVisitor<S, E> visitor) {
        return visitor.visit(this);
    }
}
