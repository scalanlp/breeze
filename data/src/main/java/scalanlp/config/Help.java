package scalanlp.config;

import java.lang.annotation.Retention;
import java.lang.annotation.RetentionPolicy;

import static java.lang.annotation.RetentionPolicy.*;

@Retention(RUNTIME)
public @interface Help {
  String text() default "";
}
