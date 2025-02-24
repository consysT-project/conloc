package de.tuda.stg.conloc.invariants.lib.crdts;


import de.tuda.stg.conloc.Mergeable;
import de.tuda.stg.conloc.annotations.invariants.ReplicatedModel;

import java.util.HashMap;
import java.util.Map;

@ReplicatedModel public class GMap<K, V extends Mergeable<V>> implements Mergeable<GMap<K, V>> {
    public Map<K, V> underlying;

    //@ ensures underlying.isEmpty();
    public GMap() {
        this.underlying = new HashMap<K, V>();
    }

    /*@
    @ assignable underlying;
    @ ensures underlying.containsKey(key);
    @ ensures (\forall K k0; !k0.equals(key); underlying.get(k0).equals(\old(underlying.get(k0))));
    @ ensures (\forall K k1; \old(underlying).containsKey(k1); underlying.containsKey(k1)); // TODO: redundant?
    @ ensures underlying.size() == \old(underlying).size() || underlying.size() == \old(underlying).size() + 1;
    @*/
    public void put(K key, V value) {
        underlying.put(key, value); // TODO: should this also merge existing keys?
    }

    //@ assignable \nothing;
    //@ ensures \result == underlying.containsKey(key);
    public boolean containsKey(K key){
        return underlying.containsKey(key);
    }

    //@ assignable \nothing;
    //@ ensures \result == underlying.containsValue(value);
    public boolean containsValue(V value){
        return underlying.containsValue(value);
    }

    //@ assignable \nothing;
    //@ ensures \result == underlying.isEmpty();
    public boolean isEmpty() {
        return underlying.isEmpty();
    }

    //@ assignable \nothing;
    //@ ensures \result == underlying.get(key); // TODO: not pure?
    public V get(K key) {
        return underlying.get(key);
    }

    public int size() {
        return underlying.size();
    }

    /*@
    @ ensures (\forall K k0; \old(underlying).containsKey(k0) || other.underlying.containsKey(k0); underlying.containsKey(k0));
    @ ensures (\forall K k1; underlying.containsKey(k1); \old(underlying).containsKey(k1) || other.underlying.containsKey(k1));
    @*/
    public Void merge(GMap<K, V> other) {
        for (K k : other.underlying.keySet()) {
            if (this.underlying.containsKey(k)) {
                V elem = this.underlying.get(k);
                elem.merge(other.underlying.get(k));
                this.underlying.put(k, elem);
            } else {
                this.underlying.put(k, other.underlying.get(k));
            }
        }
        return null;
    }
}