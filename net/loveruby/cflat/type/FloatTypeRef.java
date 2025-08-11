package net.loveruby.cflat.type;

import net.loveruby.cflat.ast.Location;
import net.loveruby.cflat.ast.TypeNode;
import net.loveruby.cflat.exception.*;

public class FloatTypeRef extends TypeRef {
    private long size;

    public FloatTypeRef(Location loc, long size) {
        super(loc);
        this.size = size;
    }

    public long size() {
        return size;
    }

    @Override
    public boolean equals(Object other) {
        if (!(other instanceof FloatTypeRef)) {
            return false;
        }
        FloatTypeRef otherFloat = (FloatTypeRef) other;
        return this.size == otherFloat.size;
    }

    @Override
    public String toString() {
        return "float64" + (size * 8);
    }
}
