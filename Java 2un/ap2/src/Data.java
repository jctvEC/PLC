public class Data 
{
	private short dia,mes,ano;
	
	public Data (short dia, short mes, short ano)
	{
		setdia(dia);
		setmes(mes);
		setano(ano);
	}
	
	public boolean vemAntes(Data data)
	{
		if (this.ano < data.ano )
			return true;
		else if(this.ano > data.ano)
			return false;
		else
		{
			if (this.mes < data.mes )
				return true;
			else if(this.mes > data.mes)
				return false;
			else
			{
				if (this.dia < data.ano )
					return true;
				else
					return false;
			}
		}
			
	}
	
	public short getdia()
	{
		return this.dia;
	}
	
	public short getmes()
	{
		return this.mes;
	}
	
	public short getano()
	{
		return this.ano;
	}
	
	public void setdia (short dia)
	{
		if (this.dia > 31 || this.dia<1)
			this.dia = 17;
		else
			this.dia = dia;
	}
	
	public void setmes(short mes)
	{
		if(this.mes < 1 || this.mes > 12)
			this.mes = 3;
		else
			this.mes = mes;
	}
	public void setano(short ano)
	{
		this.ano = ano;		
	}
	
}
