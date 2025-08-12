package net.loveruby.cflat.compiler;

import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.type.*;
import net.loveruby.cflat.utils.ErrorHandler;
import net.loveruby.cflat.exception.*;
import java.util.*;

public class TypeResolver extends Visitor
        implements EntityVisitor<Void>, DeclarationVisitor<Void> {
    // #@@range/ctor{
    private final TypeTable typeTable;
    private final ErrorHandler errorHandler;

    public TypeResolver(TypeTable typeTable, ErrorHandler errorHandler) {
        this.typeTable = typeTable;
        this.errorHandler = errorHandler;
    }
    // #@@}

    // #@@range/resolveProgram{
    public void resolve(AST ast) {
        defineTypes(ast.types());
        // #@@range/resolveProgram_core{
        for (TypeDefinition t : ast.types()) {
            t.accept(this);
        }
        for (Entity e : ast.entities()) {
            e.accept(this);
        }
        // #@@}
    }
    // #@@}

    // #@@range/defineTypes{
    private void defineTypes(List<TypeDefinition> deftypes) {
        for (TypeDefinition def : deftypes) {
            if (typeTable.isDefined(def.typeRef())) {
                error(def, "duplicated type definition: " + def.typeRef());
            } else {
                typeTable.put(def.typeRef(), def.definingType());
            }
        }
    }
    // #@@}

    // #@@range/bindType{
    private void bindType(TypeNode n) {
        System.err.println("=== bindType ===");
        System.err.println("TypeNode: " + n);
        System.err.println("TypeNode.isResolved: " + n.isResolved());
        if (n.isResolved()) {
            System.err.println("TypeNode already resolved, type: " + n.type());
            return;
        }

        System.err.println("TypeNode.typeRef: " + n.typeRef());
        System.err.println("TypeNode.typeRef type: " + n.typeRef().getClass().getSimpleName());

        Type resolvedType = typeTable.get(n.typeRef());
        System.err.println("typeTable.get result: " + resolvedType);

        n.setType(resolvedType);

        System.err.println("After setType:");
        System.err.println("TypeNode.isResolved: " + n.isResolved());
        System.err.println("TypeNode.type: " + n.type());
    }
    // #@@}

    //
    // Declarations
    //

    // #@@range/StructNode{
    public Void visit(StructNode struct) {
        resolveCompositeType(struct);
        return null;
    }
    // #@@}

    // #@@range/UnionNode{
    public Void visit(UnionNode union) {
        resolveCompositeType(union);
        return null;
    }
    // #@@}

    // #@@range/resolveCompositeType{
    public void resolveCompositeType(CompositeTypeDefinition def) {
        CompositeType ct = (CompositeType) typeTable.get(def.typeNode().typeRef());
        if (ct == null) {
            throw new Error("cannot intern struct/union: " + def.name());
        }
        for (Slot s : ct.members()) {
            bindType(s.typeNode());
        }
    }
    // #@@}

    // #@@range/TypedefNode{
    public Void visit(TypedefNode typedef) {
        bindType(typedef.typeNode());
        bindType(typedef.realTypeNode());
        return null;
    }
    // #@@}

    //
    // Entities
    //

    // #@@range/DefinedVariable{
    public Void visit(DefinedVariable var) {
        bindType(var.typeNode());
        if (var.hasInitializer()) {
            visitExpr(var.initializer());
        }
        return null;
    }
    // #@@}

    public Void visit(UndefinedVariable var) {
        bindType(var.typeNode());
        return null;
    }

    public Void visit(Constant c) {
        bindType(c.typeNode());
        visitExpr(c.value());
        return null;
    }

    // #@@range/DefinedFunction{
    public Void visit(DefinedFunction func) {
        System.err.println("=== TypeResolver.visit(DefinedFunction) ===");
        System.err.println("Function: " + func.name());
        System.err.println("Function typeNode: " + func.typeNode());
        System.err.println("Function typeNode.isResolved: " + func.typeNode().isResolved());
        if (func.typeNode().isResolved()) {
            System.err.println("Function resolved type: " + func.typeNode().type());
        }

        resolveFunctionHeader(func);

        System.err.println("After resolveFunctionHeader:");
        System.err.println("Function typeNode.isResolved: " + func.typeNode().isResolved());
        if (func.typeNode().isResolved()) {
            System.err.println("Function resolved type: " + func.typeNode().type());
            if (func.typeNode().type().isFunction()) {
                System.err.println("Function type is callable: " + func.typeNode().type().isCallable());
            }
        }

        visitStmt(func.body());
        return null;
    }
    // #@@}

    public Void visit(UndefinedFunction func) {
        System.err.println("=== TypeResolver.visit(UndefinedFunction) ===");
        System.err.println("Function: " + func.name());
        System.err.println("Function typeNode: " + func.typeNode());
        System.err.println("Function typeNode.isResolved: " + func.typeNode().isResolved());

        resolveFunctionHeader(func);

        System.err.println("After resolveFunctionHeader:");
        System.err.println("Function typeNode.isResolved: " + func.typeNode().isResolved());
        if (func.typeNode().isResolved()) {
            System.err.println("Function resolved type: " + func.typeNode().type());
        }

        return null;
    }

    // #@@range/resolveFunctionHeader{
    private void resolveFunctionHeader(Function func) {
        System.err.println("=== resolveFunctionHeader ===");
        System.err.println("Function: " + func.name());
        System.err.println("Before bindType:");
        System.err.println("  typeNode: " + func.typeNode());
        System.err.println("  typeNode.isResolved: " + func.typeNode().isResolved());
        if (func.typeNode().isResolved()) {
            System.err.println("  typeNode.type: " + func.typeNode().type());
        }

        bindType(func.typeNode());

        System.err.println("After bindType:");
        System.err.println("  typeNode.isResolved: " + func.typeNode().isResolved());
        if (func.typeNode().isResolved()) {
            System.err.println("  typeNode.type: " + func.typeNode().type());
            if (func.typeNode().type().isFunction()) {
                System.err.println(
                        "  Function type returnType: " + func.typeNode().type().getFunctionType().returnType());
                System.err.println(
                        "  Function type paramTypes: " + func.typeNode().type().getFunctionType().paramTypes());
            }
        }

        for (Parameter param : func.parameters()) {
            System.err.println("Processing parameter: " + param.name());
            System.err.println("  param.typeNode: " + param.typeNode());
            System.err.println("  param.typeNode.isResolved: " + param.typeNode().isResolved());

            // arrays must be converted to pointers in a function parameter.
            Type t = typeTable.getParamType(param.typeNode().typeRef());
            System.err.println("  getParamType result: " + t);
            param.typeNode().setType(t);

            System.err.println("  After setType:");
            System.err.println("    param.typeNode.isResolved: " + param.typeNode().isResolved());
            System.err.println("    param.typeNode.type: " + param.typeNode().type());
        }
    }
    // #@@}

    //
    // Expressions
    //

    public Void visit(BlockNode node) {
        for (DefinedVariable var : node.variables()) {
            var.accept(this);
        }
        visitStmts(node.stmts());
        return null;
    }

    public Void visit(VarDeclStmtNode node) {
        // 解析变量声明语句中的变量类型
        for (DefinedVariable var : node.variables()) {
            var.accept(this);
        }
        return null;
    }

    public Void visit(CastNode node) {
        bindType(node.typeNode());
        super.visit(node);
        return null;
    }

    public Void visit(SizeofExprNode node) {
        bindType(node.typeNode());
        super.visit(node);
        return null;
    }

    public Void visit(SizeofTypeNode node) {
        bindType(node.operandTypeNode());
        bindType(node.typeNode());
        super.visit(node);
        return null;
    }

    public Void visit(IntegerLiteralNode node) {
        bindType(node.typeNode());
        return null;
    }

    public Void visit(StringLiteralNode node) {
        bindType(node.typeNode());
        return null;
    }

    public Void visit(FloatLiteralNode node) {
        bindType(node.typeNode());
        return null;
    }

    private void error(Node node, String msg) {
        errorHandler.error(node.location(), msg);
    }
}
