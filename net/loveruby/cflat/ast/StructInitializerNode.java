package net.loveruby.cflat.ast;
import net.loveruby.cflat.type.Type;
import java.util.List;

public class StructInitializerNode extends ExprNode {
    protected Location location;
    protected String typeName; // 结构体类型名称，如 "Point"
    protected List<ExprNode> initializers;
    protected Type expectedType; // 期望的结构体类型

    public StructInitializerNode(Location loc, String typeName, List<ExprNode> initializers) {
        this.location = loc;
        this.typeName = typeName;
        this.initializers = initializers;
    }

    public Location location() {
        return location;
    }

    public String typeName() {
        return typeName;
    }

    public List<ExprNode> initializers() {
        return initializers;
    }

    public void setExpectedType(Type type) {
        this.expectedType = type;
    }

    public Type expectedType() {
        return expectedType;
    }

    public Type type() {
        return expectedType;
    }

    public <S, E> E accept(ASTVisitor<S, E> visitor) {
        return visitor.visit(this);
    }

    protected void _dump(Dumper d) {
        d.printNodeList("initializers", initializers);
    }
}