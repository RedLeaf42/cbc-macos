package net.loveruby.cflat.asm;

public enum Type {
    INT8, INT16, INT32, INT64, FLOAT32, FLOAT64,
    STRUCT; // 通用结构体类型

    static public Type get(long size) {
        switch ((int) size) {
            case 1:
                return INT8;
            case 2:
                return INT16;
            case 4:
                return INT32;
            case 8:
                return INT64;
            default:
                // 对于结构体和其他大类型，返回通用结构体类型
                return STRUCT;
        }
    }

    static public Type getFloatType(long size) {
        switch ((int) size) {
            case 4:
                return FLOAT32;
            case 8:
                return FLOAT64;
            default:
                throw new Error("unsupported float type size: " + size);
        }
    }

    public int size() {
        switch (this) {
            case INT8:
                return 1;
            case INT16:
                return 2;
            case INT32:
                return 4;
            case INT64:
                return 8;
            case FLOAT32:
                return 4;
            case FLOAT64:
                return 8;
            case STRUCT:
                // 结构体的大小是动态的，这里返回一个默认值
                // 实际使用时应该通过其他方式获取真实大小
                return 8; // 默认返回指针大小
            default:
                throw new Error("must not happen");
        }
    }

    public boolean isFloat() {
        return this == FLOAT32 || this == FLOAT64;
    }

    public boolean isInteger() {
        return this == INT8 || this == INT16 || this == INT32 || this == INT64;
    }
}
