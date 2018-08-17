
public class Serie 
{
	
	private int x0,mult,adicional;
	
	public Serie (int x0, int mult, int adicional)
	{
		this.x0 = x0;
		this.mult = mult;
		this.adicional = adicional;
	}
	
	public void imprimir(int n)
	{
		System.out.println(n);
		for(int i = 0 ; i < n; i++)
		{
			System.out.print(this.x0+" ");
			this.x0 = ((this.x0 + this.adicional)*this.mult);
		}
	}
}
