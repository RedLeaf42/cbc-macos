package net.loveruby.cflat.type;
import net.loveruby.cflat.ast.Slot;
import net.loveruby.cflat.ast.Location;
import net.loveruby.cflat.utils.AsmUtils;
import java.util.*;

public class ClassType extends CompositeType {
    public ClassType(String name, List<Slot> membs, Location loc) {
        super(name, membs, loc);
    }

    public boolean isClass() { return true; }

    public boolean isSameType(Type other) {
        if (! other.isClass()) return false;
        return equals(other.getClassType());
    }

    protected void computeOffsets() {
        long offset = 0;
        long maxAlign = 1;
        for (Slot s : members()) {
            offset = AsmUtils.align(offset, s.allocSize());
            s.setOffset(offset);
            offset += s.allocSize();
            maxAlign = Math.max(maxAlign, s.alignment());
        }
        cachedSize = AsmUtils.align(offset, maxAlign);
        cachedAlign = maxAlign;
    }

    public String toString() {
        return "class " + name;
    }
}