// gcc bboxbreaker.c -o bboxbreaker -std=c99 -g -lssl -Wall -Wextra

/* On cherche la chaîne dont les 3 derniers octets du SHA correspondent aux trois chiffres hexa du numéro de série
   Les chaînes ont la forme CPXXYYZ1Z2Z3, où
   XX : année, 08-10
   YY : semaine, 01-52, en décimal
   Z1,Z2,Z3 : de 30 à 39 inclus ou de 41 à 5A en hexa
 */
#include <stdio.h>
#include <openssl/sha.h>

// la chaîne sur laquelle on travaille
unsigned char try[] = "CPXXYYZ1Z2Z3";
// années possible
char *possible_XX[] = {"08","09","10"};
unsigned char sha[SHA_DIGEST_LENGTH];
// les octets avec lesquels comparer, lus par scanf
unsigned int obj1 = 0xF4;
unsigned int obj2 = 0x7F;
unsigned int obj3 = 0x96;

int main ()
{
	// entrée du numéro de série
	printf("Entrez le numero de serie, en espacant les octets.\n");
	printf("Ex : pour Bbox-F47F96 entrez F4 7F 96.\n");
	scanf("%x%x%x",&obj1,&obj2,&obj3);
	//XX, années
	for(int i = 0;i<2;i++){
		try[2] = possible_XX[i][0];
		try[3] = possible_XX[i][1];
		//YY, mois
		for(int j = 1;j<53;j++){
			try[4] = '0' + j / 10;
			try[5] = '0' + j % 10;
			//Z1
			for(int l = 0x30; l < 0x5B;l++) {
				if(l > 0x39 && l < 0x41)
					continue;

				try[6] = '0' + l / 16;
				try[7] = '0' + l % 16;
				if(try[7] > '9')
					try[7] += 7;
				//Z2
				for(int m = 0x30; m < 0x5B;m++) {
					if(m > 0x39 && m < 0x41)
						continue;

					try[8] = '0' + m / 16;
					try[9] = '0' + m % 16;
					if(try[9] > '9')
						try[9] += 7;
					//Z3
					for(int n = 0x30; n < 0x5B;n++) {
						if(n > 0x39 && n < 0x41)
							continue;

						try[10] = '0' + n / 16;
						try[11] = '0' + n % 16;
						if(try[11] > '9')
							try[11] += 7;

						SHA1(try,12,sha);
						if(sha[17] == obj1 && sha[18] == obj2 && sha[19] == obj3) {
							printf("Match found:\n");
							printf("%X%X%X%X%X\n",sha[0],sha[1],sha[2],sha[3],sha[4]);
						}
					}
				}
			}
		}
	}
}
