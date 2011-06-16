package jgo.runtime.internal;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

@Documented
@Retention(RUNTIME)
@Target({FIELD, LOCAL_VARIABLE, PARAMETER})
public @interface Unsigned { }
