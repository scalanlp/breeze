package breeze.config;

import java.lang.annotation.Retention;

import static java.lang.annotation.RetentionPolicy.*;

/**
 * You can add this annotation to a parameter in
 * a case class that the GenerateHelp class can process.
 */
@Retention(RUNTIME)
public @interface Help {
  String text() default "";
}
