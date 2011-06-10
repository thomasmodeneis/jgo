package jgo.runtime;

import java.lang.annotation.*;
import static java.lang.annotation.ElementType.*;
import static java.lang.annotation.RetentionPolicy.*;

@Documented
@Retention(RUNTIME)
@Target({METHOD, FIELD, LOCAL_VARIABLE, PARAMETER})
public @interface GoType {
	public String value();
}
