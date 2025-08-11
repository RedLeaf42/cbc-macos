package net.loveruby.cflat.type;

public class FloatType extends Type {
    private long size;
    private long alignment;

    public FloatType(long size) {
        this.size = size;
        this.alignment = size;
    }

    @Override
    public long size() {
        return size;
    }

    @Override
    public long alignment() {
        return alignment;
    }

    @Override
    public boolean isFloat() {
        return true;
    }

    @Override
    public boolean isScalar() {
        return true;
    }

    @Override
    public boolean isSameType(Type other) {
        if (!(other instanceof FloatType)) {
            return false;
        }
        FloatType otherFloat = (FloatType) other;
        return this.size == otherFloat.size;
    }

    @Override
    public boolean isCompatible(Type other) {
        return isSameType(other);
    }

    @Override
    public boolean isCastableTo(Type target) {
        // 只允许 float 转换为 long
        if (target.isInteger() && target.size() == 8) { // long 类型
            return true;
        }
        return false;
    }

    public String toString() {
        return "float" + (size * 8);
    }
}
