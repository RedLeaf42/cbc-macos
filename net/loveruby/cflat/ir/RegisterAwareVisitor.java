package net.loveruby.cflat.ir;

import net.loveruby.cflat.sysdep.arm64.Register;

/**
 * 寄存器感知的访问者接口
 * 允许父表达式为子表达式分配寄存器
 */
public interface RegisterAwareVisitor<S, E> {

    /**
     * 访问表达式，将结果写入指定寄存器
     * 
     * @param expr           要访问的表达式
     * @param targetRegister 目标寄存器
     * @return 访问结果
     */
    E visit(Expr expr, Register targetRegister);

    /**
     * 访问语句，不需要返回值寄存器
     * 
     * @param stmt 要访问的语句
     * @return 访问结果
     */
    E visit(Stmt stmt);

    // ====== 具体的语句处理方法 ======

    /**
     * 访问赋值语句
     */
    E visit(Assign stmt);

    /**
     * 访问表达式语句
     */
    E visit(ExprStmt stmt);

    /**
     * 访问条件跳转语句
     */
    E visit(CJump stmt);

    /**
     * 访问无条件跳转语句
     */
    E visit(Jump stmt);

    /**
     * 访问switch语句
     */
    E visit(Switch stmt);

    /**
     * 访问标签语句
     */
    E visit(LabelStmt stmt);

    /**
     * 访问返回语句
     */
    E visit(Return stmt);
}
