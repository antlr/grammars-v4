public class TryWithResourceDemo implements AutoCloseable{
	public static void main(String[] args){
		TryWithResourceDemo demo=new TryWithResourceDemo();
		try(demo){demo.doSomething();}
		/* Prior to Java 9, you should write something like
			try(TryWithResourceDemo demo=new TryWithResourceDemo()){demo.doSomething();}
		*/
	}
	public void doSomething(){
		System.out.println("Hello world!");
	}
	@Override
	public void close(){
		System.out.println("I am going to be closed");
	}
}
