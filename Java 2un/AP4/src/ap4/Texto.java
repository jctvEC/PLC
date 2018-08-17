package ap4;

public class Texto 
{
    private String str, strsplitado[];
    private int n=0;
    
    
    public Texto(String str)
    {
      this.str= str;
      qini(); // inicializando o tamanho
      splita(); //inicializo a str splitada
    }
    
    public void imprimir()
	{
    	System.out.println("Texto= "+this.str);    	
	}
    
    public void qtdpalavras()
    {
		System.out.println("Total de palavras do texto "+this.n+".");
    }
    
    public void qtdsubstr(String str2)
    {
    	int cont=0;
    	
    	for(int i=0;i<this.n;i++)
    	{
    		if(this.strsplitado[i].contains(str2))
    			{
    				cont++;
    			}    		
    	}
    	System.out.println("A palavra "+str2+" apareceu "+cont+"x no texto.");
    }
    
    
    public void substitui(String str1,String str2)
    {	
    	System.out.println("Caso encontre "+str1+" substitue por "+str2+".");
    	
    	this.str = "";
    	for(int i=0;i<this.n;i++)
    	{
    		if(this.strsplitado[i].equals(str1))
    		{
    			this.str= this.str+str2+" ";
    			this.strsplitado[i]= str2;
    		}
    		else
    			this.str= this.str+strsplitado[i]+" ";
    	}
    	System.out.println("Nova String= "+this.str);    	
    }
 
    public void splita()
    {
    	this.strsplitado= this.str.split(" ");
    }
    
    public void qini()
    {
		if( this.str.isEmpty() == false)
        {
			for(int i = 0; i < str.length(); i++)
	        {
	        	if (this.str.charAt(i) == ' ')
	                this.n++;
	        }	
			this.n++;
		}
    }
    
}