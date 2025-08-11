package net.loveruby.cflat.asm;

public enum Type {
    INT8, INT16, INT32, INT64, FLOAT32, FLOAT64;

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
                throw new Error("unsupported asm type size: " + size);
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
