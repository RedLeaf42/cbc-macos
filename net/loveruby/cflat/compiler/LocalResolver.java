package net.loveruby.cflat.compiler;

import net.loveruby.cflat.ast.*;
import net.loveruby.cflat.entity.*;
import net.loveruby.cflat.utils.ErrorHandler;
import net.loveruby.cflat.exception.*;
import java.util.*;

public class LocalResolver extends Visitor {
    // #@@range/ctor{
    private final LinkedList<Scope> scopeStack;
    private final ConstantTable constantTable;
    private final ErrorHandler errorHandler;

    public LocalResolver(ErrorHandler h) {
        this.errorHandler = h;
        this.scopeStack = new LinkedList<Scope>();
        this.constantTable = new ConstantTable();
    }
    // #@@}

    private void resolve(StmtNode n) {
        n.accept(this);
    }

    private void resolve(ExprNode n) {
        n.accept(this);
    }

    // #@@range/resolve{
    public void resolve(AST ast) throws SemanticException {
        ToplevelScope toplevel = new ToplevelScope();
        scopeStack.add(toplevel);

        // #@@range/declareToplevel{
        for (Entity decl : ast.declarations()) {
            toplevel.declareEntity(decl);
        }
        for (Entity ent : ast.definitions()) {
            toplevel.defineEntity(ent);
        }
        // #@@}
        // #@@range/resolveRefs{
        resolveGvarInitializers(ast.definedVariables());
        resolveConstantValues(ast.constants());
        resolveFunctions(ast.definedFunctions());
        // #@@}
        toplevel.checkReferences(errorHandler);
        if (errorHandler.errorOccured()) {
            throw new SemanticException("compile failed.");
        }

        ast.setScope(toplevel);
        ast.setConstantTable(constantTable);
    }
    // #@@}

    // #@@range/resolveGvarInitializers{
    private void resolveGvarInitializers(List<DefinedVariable> gvars) {
        for (DefinedVariable gvar : gvars) {
            if (gvar.hasInitializer()) {
                resolve(gvar.initializer());
            }
        }
    }
    // #@@}

    private void resolveConstantValues(List<Constant> consts) {
        for (Constant c : consts) {
            resolve(c.value());
        }
    }

    // #@@range/resolveFunctions{
    private void resolveFunctions(List<DefinedFunction> funcs) {
        for (DefinedFunction func : funcs) {
            pushScope(func.parameters());
            resolve(func.body());
            func.setScope(popScope());
        }
    }
    // #@@}

    // #@@range/BlockNode{
    public Void visit(BlockNode node) {
        System.err.println("=== BlockNode.visit() ===");
        System.err.println("Block variables count: " + node.variables().size());
        for (DefinedVariable var : node.variables()) {
            System.err.println("  Block variable: " + var.name() + " (typeNode: " + var.typeNode() + ")");
        }
        System.err.println("Current scope stack size: " + scopeStack.size());
        if (!scopeStack.isEmpty()) {
            System.err.println("Current scope: " + currentScope().getClass().getSimpleName());
        }

        pushScope(node.variables());
        System.err.println("After pushScope, scope stack size: " + scopeStack.size());
        System.err.println("New scope: " + currentScope().getClass().getSimpleName());

        super.visit(node);

        LocalScope poppedScope = popScope();
        System.err.println("After popScope, scope stack size: " + scopeStack.size());
        System.err.println("Popped scope variables: " + poppedScope.localVariables().size());
        for (DefinedVariable var : poppedScope.localVariables()) {
            System.err.println("  Popped variable: " + var.name());
        }

        node.setScope(poppedScope);
        return null;
    }
    // #@@}

    // #@@range/pushScope{
    private void pushScope(List<? extends DefinedVariable> vars) {
        System.err.println("=== pushScope() ===");
        System.err.println("Variables to add: " + vars.size());
        for (DefinedVariable var : vars) {
            System.err.println("  Adding variable: " + var.name() + " (typeNode: " + var.typeNode() + ")");
        }

        LocalScope scope = new LocalScope(currentScope());
        System.err.println("Created new LocalScope with parent: "
                + (currentScope() != null ? currentScope().getClass().getSimpleName() : "null"));

        for (DefinedVariable var : vars) {
            if (scope.isDefinedLocally(var.name())) {
                System.err.println("ERROR: Duplicate variable: " + var.name());
                error(var.location(),
                        "duplicated variable in scope: " + var.name());
            } else {
                System.err.println("Defining variable: " + var.name() + " in scope");
                scope.defineVariable(var);
            }
        }

        scopeStack.addLast(scope);
        System.err.println("Added scope to stack. Stack size now: " + scopeStack.size());

        // 打印当前作用域中的所有变量
        System.err.println("Current scope variables:");
        for (DefinedVariable var : scope.localVariables()) {
            System.err.println("  " + var.name() + " (typeNode: " + var.typeNode() + ")");
        }
    }
    // #@@}

    // #@@range/popScope{
    private LocalScope popScope() {
        System.err.println("=== popScope() ===");
        System.err.println("Before pop, scope stack size: " + scopeStack.size());
        LocalScope scope = (LocalScope) scopeStack.removeLast();
        System.err.println("After pop, scope stack size: " + scopeStack.size());
        System.err.println("Popped scope variables: " + scope.localVariables().size());
        return scope;
    }
    // #@@}

    // #@@range/currentScope{
    private Scope currentScope() {
        if (scopeStack.isEmpty()) {
            System.err.println("ERROR: Scope stack is empty!");
            return null;
        }
        Scope scope = scopeStack.getLast();
        System.err.println("Current scope: " + scope.getClass().getSimpleName());
        return scope;
    }
    // #@@}

    // #@@range/StringLiteralNode{
    public Void visit(StringLiteralNode node) {
        node.setEntry(constantTable.intern(node.value()));
        return null;
    }
    // #@@}

    // #@@range/FloatLiteralNode{
    public Void visit(FloatLiteralNode node) {
        // 浮点数字面量不需要特殊处理，直接返回null
        return null;
    }
    // #@@}

    // #@@range/VariableNode{
    public Void visit(VariableNode node) {
        System.err.println("=== VariableNode.visit() ===");
        System.err.println("Looking for variable: " + node.name());
        System.err.println("Current scope stack size: " + scopeStack.size());
        if (!scopeStack.isEmpty()) {
            System.err.println("Current scope: " + currentScope().getClass().getSimpleName());
        }

        try {
            Entity ent = currentScope().get(node.name());
            System.err.println("Found entity: " + ent.name() + " (typeNode: " + ent.typeNode() + ")");
            ent.refered();
            node.setEntity(ent);
        } catch (SemanticException ex) {
            System.err.println("ERROR: Cannot resolve variable: " + node.name());
            System.err.println("Error message: " + ex.getMessage());
            System.err.println("Current scope variables:");
            if (!scopeStack.isEmpty()) {
                Scope scope = currentScope();
                if (scope instanceof LocalScope) {
                    LocalScope localScope = (LocalScope) scope;
                    for (DefinedVariable var : localScope.localVariables()) {
                        System.err.println("  " + var.name() + " (typeNode: " + var.typeNode() + ")");
                    }
                }
            }
            error(node, ex.getMessage());
        }
        return null;
    }
    // #@@}

    private void error(Node node, String message) {
        errorHandler.error(node.location(), message);
    }

    private void error(Location loc, String message) {
        errorHandler.error(loc, message);
    }
}
