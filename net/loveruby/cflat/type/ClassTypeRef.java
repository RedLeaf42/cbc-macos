package net.loveruby.cflat.type;
import net.loveruby.cflat.ast.Location;

public class ClassTypeRef extends TypeRef {
    protected String name;

    public ClassTypeRef(String name) {
        this(null, name);
    }

    public ClassTypeRef(Location loc, String name) {
        super(loc);
        this.name = name;
    }

    public boolean isClass() {
        return true;
    }

    public boolean equals(Object other) {
        if (!(other instanceof ClassTypeRef)) return false;
        return name.equals(((ClassTypeRef)other).name);
    }

    public String name() {
        return name;
    }

    public String toString() {
        return "class " + name;
    }
}