// import from http://www.simeji.com/wiki/index.php?Java%A4%CE%CD%AB%DD%B5#navigator
import java.io.*;
public class ShowEncoding {
  public static void main(String[] args) {
      System.out.println("Default encoding: " +
          new InputStreamReader(System.in).getEncoding());
      System.out.println("file.encoding: " +
        java.lang.System.getProperty("file.encoding"));
  }
}
