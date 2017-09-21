
package exercise8

object Exercise8_3 extends App {
  trait Prop {

    def check: Boolean

//    def &&(p: Prop): Prop = {
//      this.check && p.check
//    }

    /* We can refer to the enclosing `Prop` instance with `Prop.this` */
    def &&(p: Prop): Prop = new Prop {
      def check = Prop.this.check && p.check
    }
  }

}
