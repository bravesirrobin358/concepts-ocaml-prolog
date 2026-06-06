import java.util.*;
import java.lang.Math;

class Shape {
	public int val = 4;
	public static void main(String[] args){
		int[] x = new int[4];
		x = new int[] {5,3,1,2};
		x = new int[] {5,2,0,7,8,5};

		int y = 4;
		double a = Math.floor(y);
		System.out.println("a = " + a);

		//a = new int[] {5,3,1};
		Shape e = new Shape();
		e.doThing();
		e = new Triangle();
		e.doThing();
		e.type().doThing();
		e.type().doOtherThing();
		Shape[] s = new Triangle[2]; 
		ArrayList<Shape> c = new ArrayList<Shape>();
		System.out.println(c.getClass().getName());
		//double rfe = 3/2;
	}
	public void doThing(){System.out.println("Shape class, method doThing");}
    public void doOtherThing(){System.out.println("Shape class, method doOtherThing");}
    Shape type(){return this;}
}


class Triangle extends Shape {
	public void doThing(){System.out.println("Triangle class, method doThing");}
	public void doOtherThing(){System.out.println("Triangle class, method doOtherThing");}
}

class Square extends Shape {
	public void doThing(){System.out.println("Square class, method doThing");}
}