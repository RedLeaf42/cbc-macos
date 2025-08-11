package net.loveruby.cflat.ast;

import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.type.Type;
import java.util.List;

public class VarDeclStmtNode extends StmtNode {
    private List<DefinedVariable> variables;

    public VarDeclStmtNode(Location loc, List<DefinedVariable> vars) {
        super(loc);
        this.variables = vars;
    }

    public List<DefinedVariable> variables() {
        return variables;
    }

    public <S, E> S accept(ASTVisitor<S, E> visitor) {
        return visitor.visit(this);
    }

    protected void _dump(Dumper d) {
        d.printNodeList("variables", variables);
    }
}
