/**
 *   Copyright (c) Rich Hickey. All rights reserved.
 *   The use and distribution terms for this software are covered by the
 *   Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
 *   which can be found in the file epl-v10.html at the root of this distribution.
 *   By using this software in any fashion, you are agreeing to be bound by
 *       the terms of this license.
 *   You must not remove this notice, or any other, from this software.
 **/

/* rich Mar 25, 2006 4:05:37 PM */

package ox.lang;
import clojure.lang.ArityException;

/** FIXME:
 * At some point in the future static Fns could probably become
 * Clojure.lang.IFn, so that if you create an instance of them
 * everything doesn't just break.
 */

public abstract class ASFn {

    public static String name;

    public static Object invokeStatic() {
        return throwStaticArity(0);
    }

    public static Object invokeStatic(Object arg1) {
        return throwStaticArity(1);
    }

    public static Object invokeStatic(Object arg1, Object arg2) {
        return throwStaticArity(2);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3) {
        return throwStaticArity(3);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4) {
        return throwStaticArity(4);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5) {
        return throwStaticArity(5);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6) {
        return throwStaticArity(6);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7)
    {
        return throwStaticArity(7);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8) {
        return throwStaticArity(8);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9) {
        return throwStaticArity(9);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10) {
        return throwStaticArity(10);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11) {
        return throwStaticArity(11);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12) {
        return throwStaticArity(12);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13)
    {
        return throwStaticArity(13);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14)
    {
        return throwStaticArity(14);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15) {
        return throwStaticArity(15);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15, Object arg16) {
        return throwStaticArity(16);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15, Object arg16, Object arg17) {
        return throwStaticArity(17);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15, Object arg16, Object arg17, Object arg18) {
        return throwStaticArity(18);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15, Object arg16, Object arg17, Object arg18, Object arg19) {
        return throwStaticArity(19);
    }

    public static Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                                Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                                Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20)
    {
        return throwStaticArity(20);
    }


    public Object invokeStatic(Object arg1, Object arg2, Object arg3, Object arg4, Object arg5, Object arg6, Object arg7,
                         Object arg8, Object arg9, Object arg10, Object arg11, Object arg12, Object arg13, Object arg14,
                         Object arg15, Object arg16, Object arg17, Object arg18, Object arg19, Object arg20,
                         Object... args)
    {
        return throwStaticArity(21);
    }

    public static Object throwStaticArity(int n){
        throw new ArityException(n, name);
    }
}
