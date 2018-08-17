package ap4;

public class Main 
{
	public static void main(String[] args) 
	{
        String str,substr,substr2;
		str= "Maria Joana Maria da SIlva Maria";
		substr = "Maria";
		substr2= "José";
		
		//Serie s1= new Serie(1,2,0); Feitos em sala
        //s1.imprimir(15);
        
		//Data d = new Data( (short) 17,(short) 03,(short) 1998 );
		//Data d2 = new Data((short) 10,(short) 02, (short) 1997);
		//boolean x;				
		//System.out.println("Dia "+d.getdia()+"Mes "+d.getmes()+"Ano"+d.getano());
		//x = d.vemAntes(d2);
		//System.out.println(x);
		
		Texto t1= new Texto(str); //AP4
        
        t1.imprimir();
        t1.qtdpalavras();
        t1.qtdsubstr(substr);
        t1.substitui(substr,substr2);
	}
		

}