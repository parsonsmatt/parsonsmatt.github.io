import java.util.function.*;
import java.util.*;

class Folds {
    static class List<T> {
        public final T head;
        public final List<T> tail;
        private List() {
            this.head = null;
            this.tail = null;
        }
        private List(T head, List<T> tail) {
            assert head != null;
            assert tail != null;
            this.head = head;
            this.tail = tail;
        } 

        public static <T> List<T> Cons(T item, List<T> rest) {
            return new List<T>(item, rest);
        }

        public static <T> List<T> Cons(T item) {
            return Cons(item, Nil());
        }

        public static <T> List<T> Nil() {
            return new List<>();
        }

        public <T> boolean isNil() {
            return head == null;
        }

        public static <T> List<T> concat(List<T> x, List<T> y) {
            return foldRight(List::Cons, y, x);
        }


        public List<T> append(T elem) {
            return foldRight(List::Cons, Cons(elem, Nil()), this);
        }
    }


    static <A, B> List<B> map(Function<A, B> f, List<A> list) {
        if (list.isNil()) {
            return List.Nil();
        }
        A val = list.head;
        List<A> rest = list.tail;
        B newVal = f.apply(val);
        List<B> newRest = map(f, rest);
        return List.Cons(newVal, newRest);
    }
    
    static <A> List<A> filter(Function<A, Boolean> predicate, List<A> list) {
        if (list.isNil()) {
            return List.Nil();
        }
        if (predicate.apply(list.head)) {
            return List.Cons(list.head, filter(predicate, list.tail));
        }

        return filter(predicate, list.tail);
    }

    static Integer sum(List<Integer> list) {
        if (list.isNil()) {
            return 0;
        }
        return list.head + sum(list.tail);
    }

    static <A, B> B foldRight(BiFunction<A, B, B> k, B z, List<A> list) {
        if (list.isNil()) {
            return z;
        }

        return k.apply(list.head, foldRight(k, z, list.tail));
    }

    static <A, B> B foldLeft(BiFunction<B, A, B> k, B z, List<A> list) {
        if (list.isNil()) {
            return z;
        }

        return foldLeft(k, k.apply(z, list.head), list.tail);
    }

    static <A, B> List<B> newMap(Function<A, B> f, List<A> list) {
        return foldRight(
                (x, acc) -> List.Cons(f.apply(x), acc), 
                List.Nil(), 
                list
            );
    }

    static <A> List<A> newFilter(Function<A, Boolean> p, List<A> list) {
        return foldRight(
                (x, acc) -> p.apply(x) ? List.Cons(x, acc) : acc,
                List.Nil(),
                list
            );
    }

    static <A, B> List<B> mapL(Function<A, B> f, List<A> list) {
        return foldLeft(
                (acc, x) -> acc.append(f.apply(x)),
                List.Nil(),
                list
            );
    }
}
