package riso.numerical;

/** Translated from <tt>http://www.netlib.org/toms/647</tt>:
  * Bennett L. Fox, "Algorithm 647: Implementation and Relative Efficiency of Quasirandom Sequence Generators",
  * ACM Transactions on Mathematical Software, vol. 12, no. 4 (December, 1986), pp 362--376.
  *
  * <p> Note that this scheme works only in 2 or more dimensions -- it cannot generate
  * a 1-dimensional sequence.
  * 
  * <p> This file is distributed under the terms of the ACM Software Copyright and License Agreement.
  * A copy of the license agreement, <tt>ACM-LICENSE.html</tt>, is included with the RISO distribution.
  */
public class LowDiscrepancy
{
	static int [ ] primes =
	{
		1, 2, 3, 5, 5, 7, 7, 11, 11, 11, 11, 13, 13, 17, 17, 17, 17, 19, 19, 
		23, 23, 23, 23, 29, 29, 29, 29, 29, 29, 31, 31, 37, 37, 37, 37,
		37, 37, 41, 41, 41
	};

	/** <tt>dimen</tt> must be at least 2.
	  */
	public static void infaur ( boolean [ ] flag, int dimen, int atmost )
	{
		faure.s=dimen;
		flag [ 1-1 ] = ( faure.s > 1 && faure.s < 41 );
		if ( ! flag [ 1-1 ] ) return;
		faure.qs=primes [ faure.s-1 ];
		faure.testn=faure.qs*faure.qs*faure.qs*faure.qs;
		faure.hisum= ( int ) ( Math.log ( atmost+faure.testn ) /Math.log ( faure.qs ) );
		flag [ 2-1 ] = ( faure.hisum < 20 );
		if ( ! flag [ 2-1 ] ) return;
		faure.coef [ 0 ] [ 0 ] =1;
		for ( int j = 1 ; j <= faure.hisum ; j++ )
		{
			faure.coef [ j ] [ 0 ] =1;
			faure.coef [ j ] [ j ] =1;
		}
		for ( int j = 1 ; j <= faure.hisum ; j++ )
		{
			for ( int i = j+1 ; i <= faure.hisum ; i++ )
			{
				faure.coef [ i ] [ j ] = ( faure.coef [ i-1 ] [ j ] +faure.coef [ i-1 ] [ j-1 ] ) % faure.qs;
			}
		}
		faure.nextn=faure.testn-1;
		faure.hisum=3;
		faure.rqs=1.0/faure.qs;
	}

	public static void gofaur ( double [ ] quasi )
	{
		int [ ] ytemp = new int [ 20 ];
		int ztemp, ktemp,ltemp,mtemp;
		double r;
		ktemp=faure.testn;
		ltemp=faure.nextn;
		for ( int i = faure.hisum ; i >= 0 ; i-- )
		{
			ktemp=ktemp/faure.qs;
			mtemp=ltemp % ktemp;
			ytemp [ i ] = ( ltemp-mtemp ) /ktemp;
			ltemp=mtemp;
		}
		r=ytemp [ faure.hisum ];
		for ( int i = faure.hisum-1 ; i >= 0 ; i-- )
		{
			r=ytemp [ i ] +faure.rqs*r;
		}
		quasi [ 1-1 ] =r*faure.rqs;
		for ( int k = 2 ; k <= faure.s ; k++ )
		{
			quasi [ k-1 ] =0;
			r=faure.rqs;
			for ( int j = 0 ; j <= faure.hisum ; j++ )
			{
				ztemp=0;
				for ( int i = j ; i <= faure.hisum ; i++ )
				{
					ztemp=ztemp+faure.coef [ i ] [ j ] *ytemp [ i ];
				}
				ytemp [ j ] =ztemp % faure.qs;
				quasi [ k-1 ] =quasi [ k-1 ] +ytemp [ j ] *r;
				r=r*faure.rqs;
			}
		}
		faure.nextn=faure.nextn+1;
		if ( faure.nextn == faure.testn )
		{
			faure.testn=faure.testn*faure.qs;
			faure.hisum=faure.hisum+1;
		}
	}

	public static void inhalt ( boolean [ ] flag, int dimen, int atmost, double tiny, double [ ] quasi )
	{
		double delta;
		halton.s=dimen;
		flag [ 1-1 ] = ( halton.s > 1 && halton.s < 41 );
		if ( ! flag [ 1-1 ] ) return;
		halton.e=0.9* ( 1.0/ ( atmost*halton.prime [ halton.s-1 ] ) -10*tiny );
		delta=100*tiny* ( atmost+1 ) * (Math.log( atmost )/Math.log(10));
		flag [ 2-1 ] = ( delta <= 0.09* ( halton.e-10*tiny ) );
		if ( ! flag [ 2-1 ] ) return;
		for ( int i = 1 ; i <= halton.s ; i++ )
		{
			halton.prime1 [ i-1 ] =1/halton.prime [ i-1 ];
			quasi [ i-1 ] =halton.prime1 [ i-1 ];
		}
	}

	public static void gohalt ( double [ ] quasi )
	{
		double t, f,g,h;
		for ( int i = 1 ; i <= halton.s ; i++ )
		{
			t=halton.prime1 [ i-1 ];
			f=1-quasi [ i-1 ];
			g=1;
			h=t;
			while ( f-h < halton.e )
			{
				g=h;
				h=h*t;
			}
			quasi [ i-1 ] =g+h-f;
		}
	}
	
	public static void main( String[] args )
	{
		boolean omit_output = false;
		boolean[] flag = new boolean[2];
		int m = 3, n = 20;

		for ( int i = 0; i < args.length; i++ )
		{
			switch (args[i].charAt(1))
			{
			case 'm':
				m = Integer.parseInt( args[++i] );
				break;
			case 'n':
				n = Integer.parseInt( args[++i] );
				break;
			case 'o':
				omit_output = true;
				break;
			}
		}

		System.err.println( "m: "+m+", n: "+n );
		double[] quasi = new double[m];

		infaur( flag, m, n );
		for ( int i = 0; i < n; i++ )
		{
			gofaur( quasi );
			if ( !omit_output )
			{
				for ( int j = 0; j < quasi.length; j++ )
					System.out.print( quasi[j]+"  " );
				System.out.println("");
			}
		}
	}
}

class halton
{
	static int s;
	static double e;
	static double [ ] prime =
	{
		2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43,
		47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97, 101, 103, 107,
		109, 113, 127, 131, 137, 139, 149, 151, 157, 163, 167, 173 
	};
    static double [ ] prime1 = new double [prime.length];
}

class faure
{
	static int s, qs, nextn, testn, hisum;
	static double rqs;
	static int [ ] [ ] coef = new int [ 20 ] [ 20 ];
}
