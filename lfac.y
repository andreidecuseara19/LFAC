%{
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>

//Esentiale
extern FILE* yyin;
extern char* yytext;
extern int yylineno;

//Ne spune daca programul compileaza corect
int ok = 1;

//De scris in fisier
int fd;
char buffer[100];

//Folosit pentru functii
int nrParametru = 0;
char numeFunctie[100];

//Functii
int cautare(char* tip, char* numeVar);
int cautaNume(char *numeVar);
char *obtineTipul(char *numeVar);
int parametriiCorecti(char *numeVar);
int schimbaValoareaNat(char *tip, char *numeVar, int valoareNat);
int schimbaValoareaReal(char *tip, char *numeVar, float valoareReal);
int schimbaValoareaSir(char *tip, char *numeVar, char *valoareSir);
int schimbaValoareaCara(char *tip, char *numeVar, char valoareCara);
int schimbaValoareaBoolean(char *tip, char *numeVar, char *valoareBool);
int cautaVal(char *numeVar);

%}
%union {
int natval;
float realval;
char* bulval;
char caraval;
char* sirval;
char* numevar;
char* tip;
}
%token main2 test natural real caracter sir boolean clasa vector functie def
daca altfel pentru catTimp executa comp eval plus minus inmultit impartit
copiaza compara lungime concateneaza
%token <natval> nr_nat
%token <realval> nr_real
%token <caraval> cara
%token <sirval> str
%token <bulval> bul
%token <numevar> nume

%type <natval> valoare_nat
%type <realval> valoare_real
%type <caraval> valoare_caracter
%type <sirval> valoare_sir
%type <bulval> valoare_boolean

%type <natval> operatie_naturala
%type <natval> operatii_naturale

%left '+' minus
%left '*' '/'
%left  UMINUS
%start s
%%

//Inceput
s : progr
{     
      if(ok == 1)
            printf("Limbaj acceptat! Well done!\n");
      else printf("Eroare de compilare! \n");
}

//Declaratii
progr : declaratii_globale main_progr 
      | main_progr
      ;
declaratii_globale : declaratii_globale declaratie    {write(fd, "global\n", strlen("global\n")); }
                   | declaratie                       {write(fd, "global\n", strlen("global\n")); }
                   ;
declaratii : declaratii declaratie                    {write(fd, "local\n", strlen("local\n"));}
          | declaratie                                {write(fd, "local\n", strlen("local\n"));}
          ;
declaratie : variabila
           | variabila_declarata
           | obiect
           | array
           | definitie
           | metoda
           ;


//Variabile (din declaratii)
variabila : natural nume                                    { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "natural %s ", $2); write(fd, buffer, strlen(buffer)); }
          | caracter nume                                   { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "caracter %s ", $2); write(fd, buffer, strlen(buffer)); }
          | real nume                                       { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "real %s ", $2); write(fd, buffer, strlen(buffer)); }
          | sir nume                                        { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "sir %s ", $2); write(fd, buffer, strlen(buffer)); }
          | boolean nume                                    { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "boolean %s ", $2); write(fd, buffer, strlen(buffer)); }
          ;
variabila_declarata : natural nume '=' valoare_nat          { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "natural %s = %d ", $2, $4); write(fd, buffer, strlen(buffer)); }
                    | real nume '=' valoare_real            { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "real %s = %.3f ", $2, $4); write(fd, buffer, strlen(buffer)); }
                    | sir nume '=' valoare_sir              { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "sir %s = %s ", $2, $4); write(fd, buffer, strlen(buffer)); }
                    | boolean nume '=' valoare_boolean      { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "boolean %s = %s ", $2, $4); write(fd, buffer, strlen(buffer)); }
                    | caracter nume '=' valoare_caracter    { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "caracter %s = %c ", $2, $4); write(fd, buffer, strlen(buffer)); }
                    ;
obiect : clasa nume  { if(cautaNume($2)) ok = 0; snprintf(buffer, 100, "clasa %s \n", $2); write(fd, buffer, strlen(buffer)); } '{' declaratii '}'              
       ;
array : vector nr_nat natural nume '=' '[' lista_de_variabile_nat ']'     { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat natural nume '=' '[' ']'                            { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat real nume '=' '[' lista_de_variabile_real ']'       { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat real nume '=' '[' ']'                               { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat caracter nume '=' '[' lista_de_variabile_cara ']'   { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat caracter nume '=' '[' ']'                           { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat boolean nume '=' '[' lista_de_variabile_boolean ']' { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      | vector nr_nat boolean nume '=' '[' ']'                            { if(cautaNume($4)) ok = 0; snprintf(buffer, 100, "vector %s ", $4); write(fd, buffer, strlen(buffer)); }
      ;
definitie : def nume nr_nat
          | def nume str
          | def nume nr_real
          | def nume cara
          | def nume bul
          ;
 
//MAIN PROGRAM
main_progr : main2'('')' acolade
           ;
acolade : '{' '}'
        | '{' cods '}'
        ;
cods : cods cod
     | cod
     ;

//Codul in sine
cod : interogari        // if-uri
    | loops             // for,while, do while

    // Apelare functie
    | nume '(' ')'      
      {
            if(cautare("functie",$1) == 0) 
            {
                  printf("functia %s nu a fost declarata \n", $1); 
                  ok = 0;
            }
      }

    // Apelare functie cu parametrii ex: f(a,b), trebuie verificata signature-ul functiei
    | nume '('          
       {
             nrParametru=0; 
             strcpy(numeFunctie,$1);
       } 
      lista_variabile ')' 


    // Atribuire, verificam daca au acelasi tip si sunt initializate
    | nume '=' nume     
    {  
      char *tip1 = obtineTipul($1);  if(tip1 == NULL) { printf("variabila %s e neinitializata \n", $1); ok = 0; }
      char *tip2 = obtineTipul($3);  if(tip2 == NULL) { printf("variabila %s e neinitializata \n", $3); ok = 0; }
      if(tip1 && tip2 && strcmp(tip1,tip2) != 0) {printf("variabilele %s si %s nu sunt de acelasi tip \n", $1, $3); ok = 0;     }
    }

    // declaratii locale in cod
    | declaratii

    // atribuirea la naturale (cauta daca a fost declarata, apoi actualizeaza simboluri.txt)
    | nume '=' valoare_nat  { 
                              if(cautare("natural", $1) == 0) 
                              {ok = 0; printf("eroare la variabila %s \n", $1);} 
                              schimbaValoareaNat("natural", $1, $3);
                            }

    // atribuire la reale
    | nume '=' valoare_real  {
                               if(cautare("real", $1) == 0) 
                               {ok = 0; printf("eroare la variabila %s \n", $1);}
                               schimbaValoareaReal("real", $1, $3);

                             }

    // atribuire la sir
    | nume '=' valoare_sir  { 
                              if(cautare("sir", $1) == 0)
                              {ok = 0; printf("eroare la variabila %s \n", $1);} 
                              schimbaValoareaSir("sir", $1, $3);
                            }

    // atribuire la caracter
    | nume '=' valoare_caracter { 
                                 if(cautare("caracter", $1) == 0) 
                                 {ok = 0; printf("eroare la variabila %s \n", $1);} 
                                 schimbaValoareaCara("caracter", $1, $3);
                                }

    // atribuire la boolean
    | nume '=' valoare_boolean { 
                                if(cautare("boolean", $1) == 0) 
                                {ok = 0; printf("eroare la variabila %s \n", $1);}
                                schimbaValoareaBoolean("boolean", $1, $3);
                               }

      // atribuirea unei expresii la o variabila naturala
    | nume '=' operatii_naturale{
                                  if(cautare("natural", $1) == 0) 
                                  {ok = 0; printf("eroare la variabila %s \n", $1);} 
                                  schimbaValoareaNat("natural", $1, $3);
                                } 

      //functia eval(expr)
    | eval '(' operatii_naturale ')' {printf("avem valoarea %d \n", $3);}
    ;


// Calcul real al unei expresii
operatii_naturale : operatie_naturala {$$ = $1;}
                  | operatii_naturale '+' operatii_naturale { $$ = $1 + $3; }
                  | operatii_naturale minus operatii_naturale { $$ = $1 - $3; }
                  | operatii_naturale '*' operatii_naturale { $$ = $1 * $3; }
                  | operatii_naturale '/' operatii_naturale { $$ = $1 / $3; }
                  ;
operatie_naturala : 
                  //Verificam daca variabila este initializata si de tipul int, si ii obtinem valoarea din tabel
                  nume 
                  {
                        char *tip = obtineTipul($1); 
                        if (tip == NULL)
                        {
                              printf("variabila %s nu a fost initializat\n", $1);
                              ok = 0;
                        }
                        if (tip && strcmp(tip, "natural") != 0)
                        {
                              printf("variabila %s nu este de tipul natural\n", $1);
                              ok = 0;
                        }
                        if (tip && strcmp(tip, "natural") == 0 ) 
                        {
                              printf("vom returna %s\n",$1);
                              $$ =  cautaVal($1);
                        }
                  }

                  //Atribuim valoarea nr-ul natural
                  | nr_nat {$$ = $1;}
                  ;


// FOlosit la array
lista_variabile : lista_variabile ',' nume { nrParametru++; parametriiCorecti($3); }
                | nume { nrParametru++; parametriiCorecti($1); }
                ;

// If-uri
interogari : interogari interogare
           | interogare
           ;
interogare : daca '(' conditie ')' acolade
           | daca '(' conditie ')' acolade altfel acolade
           ;
conditie : nume comp nume
         | nume comp nr_nat
         | nume comp nr_real
         | compara '(' nume ',' nume ')'
         ;

//While,for, doWhile
loops : loops loop
      | loop
      ;
loop : pentru variabila_declarata ',' nume ',' nr_nat acolade
     | pentru variabila_declarata ',' nr_nat ',' nr_nat acolade
     | pentru variabila_declarata ',' caracter ',' nr_nat acolade
     | pentru variabila_declarata ',' nr_real ',' nr_real acolade
     | pentru variabila_declarata ',' nume ',' nr_real acolade
     | catTimp conditie acolade
     | executa acolade catTimp conditie
     ;

//Functie declarata
metoda: 
      // functie fara parametrii
      functie nume '(' ')'  
      {
            if(cautaNume($2) == 1) 
                  ok = 0;
            snprintf(buffer, 100,"functie %s: \n", $2); 
            write(fd, buffer, strlen(buffer));
      } 
      acolade 

      // functie cu parametrii
      | functie nume '('
      {
            if(cautaNume($2) == 1) 
                  ok = 0;
            snprintf(buffer, 100,"functie %s: \n", $2); 
            write(fd, buffer, strlen(buffer));
      } 
       declaratii ')' acolade 
      ;

//pentru numere naturale
valoare_nat : nr_nat {$$=$1;}
            ;
lista_de_variabile_nat : nr_nat
                       | lista_de_variabile_nat ',' nr_nat
                       ;

//pentru numere reale
valoare_real : nr_real {$$=$1;}
             ;
lista_de_variabile_real : nr_real
                       | lista_de_variabile_real ',' nr_real
                       ;

//pentru caractere
valoare_caracter : cara {$$=$1;}
                 ;
lista_de_variabile_cara : cara
                       | lista_de_variabile_cara ',' cara
                       ;


//pentru boolean
valoare_boolean : bul {$$=$1;}
            ;
lista_de_variabile_boolean : bul
                       | lista_de_variabile_boolean ',' bul
                       ;


//pentru sir
valoare_sir : str {$$=$1;}
            | copiaza '(' nume ')'
            | concateneaza '(' nume ',' nume ')'
            ;
lista_de_variabile_sir: str
                      | lista_de_variabile_sir ',' str
                      ;
%%
int yyerror(char * s){
printf("eroare de sintaxa: %s la linia:%d\n",s,yylineno);
}

int cautare(char* tip, char* numeVar)
{     
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  linie[strlen(linie) - 6] = '\0';      
            else if(strstr(linie, "global"))  
                  linie[strlen(linie) - 7] = '\0';   
            else linie[strlen(linie) - 2] = '\0';

            //Scoatem "="
            if(strstr(linie,"="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Comparam
            //printf("comparam [%s] cu [%s] \n", linie, variabila);
            if (strcmp(linie, variabila) == 0)
                  gasit = 1;
      }

      return gasit;
}

int cautaNume(char *numeVar)
{
      int flag = 0;
      flag += cautare("natural",numeVar);
      flag += cautare("real",numeVar);
      flag += cautare("sir",numeVar);
      flag += cautare("caracter",numeVar);
      flag += cautare("boolean",numeVar);
      flag += cautare("functie", numeVar);
      flag += cautare("vector", numeVar);
      flag += cautare("clasa", numeVar);
      if(flag == 0)
            return 0;
      printf("variabila %s deja exista \n" ,numeVar);
      return 1;
}

char *obtineTipul(char *numeVar)
{
      char *p = (char *)malloc(20);

      if( cautare("natural",numeVar) )
            strcpy(p, "natural");
      else if( cautare("real",numeVar) )
            strcpy(p, "real");
      else if( cautare("sir",numeVar) )
            strcpy(p, "sir");
      else if( cautare("boolean",numeVar) )
            strcpy(p, "boolean");
      else if( cautare("caracter",numeVar) )
            strcpy(p, "caracter");
      else if ( cautare("functie",numeVar) )
            strcpy(p, "functie");
      else if( cautare("vector",numeVar) )
            strcpy(p, "vector");
      else if( cautare("clasa", numeVar) )
            strcpy(p, "clasa");
      else  p = NULL;

      return p;
}

int parametriiCorecti(char *numeVar)
{
      //Cautam functia in fisier

      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;

      char variabila[100];
      strcpy(variabila, "functie ");
      strcat(variabila, numeFunctie);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  linie[strlen(linie) - 6] = '\0';      
            else if(strstr(linie, "global"))  
                  linie[strlen(linie) - 7] = '\0';   
            else linie[strlen(linie) - 2] = '\0';

            //Daca am gasit functia in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Sarim peste declaratia functiei
                  bzero(linie, 100);
                  index = 0;
                  while(1)
                  {
                        read(fd, &c, 1);
                        if(c == '\n')
                        break;                        
                        linie[index++] = (char )c;                   
                  }

                  // Parcurgem fiecare linie (adica fiecare parametru)
                  int parametru = 1;
                  while(parametru < nrParametru)
                  {
                        //citim nrParametru linii
                        bzero(linie, 100);
                        index = 0;
                        while(1)
                        {
                              read(fd, &c, 1);
                              if(c == '\n')
                                    break;                        
                              linie[index++] = (char )c;                   
                        }
                        parametru ++;
                  }
                  
                  //Testam daca ce am introdus noi ca parametru este de acelasi tip
                  char *tipVariabila = obtineTipul(numeVar);
                  char *tipParametru = strtok(linie, " ");
                  if (!tipVariabila)
                  {
                        printf("variabila %s nu a fost declarata! \n", numeVar);
                        ok = 0;
                  }
                  if(tipVariabila && strcmp(tipVariabila,tipParametru) != 0)
                  {
                        printf("variabila %s nu este de tipul %s! \n", numeVar, tipParametru);
                        ok = 0 ;
                  }
            }
      }
      return 0;
}

int schimbaValoareaNat(char *tip, char *numeVar, int valoareNat)
{
      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;
      char scope[10];

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;

            //Vrem sa stim cat dam cursorul inapoi
            int inapoi = strlen(linie) + 1;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  {linie[strlen(linie) - 6] = '\0'; strcpy(scope,"local");}     
            else if(strstr(linie, "global"))  
                  {linie[strlen(linie) - 7] = '\0'; strcpy(scope,"global");}

            //Scoatem "="
            if( strstr(linie, "="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Daca am gasit variabila in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Copiem tot ce e dupa linie (si are potential sa fie suprascris)
                  char rest[10000];
                  bzero(rest, 10000);
                  pread(fd, rest, 10000, lseek(fd,0,SEEK_CUR));

                  //Creem noua linie
                  lseek(fd, -inapoi, SEEK_CUR);
                  char temp[50];
                  strcpy(temp, linie);
                  snprintf(linie,100,"%s = %d %s",temp, valoareNat, scope);

                  //Suprascriem
                  write(fd, linie, strlen(linie));
                  write(fd,"\n",1);
                  write(fd, rest, strlen(rest));
                  return 0;
            }
      }
}

int schimbaValoareaReal(char *tip, char *numeVar, float valoareReal)
{
      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;
      char scope[10];

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;

            //Vrem sa stim cat dam cursorul inapoi
            int inapoi = strlen(linie) + 1;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  {linie[strlen(linie) - 6] = '\0'; strcpy(scope,"local");}     
            else if(strstr(linie, "global"))  
                  {linie[strlen(linie) - 7] = '\0'; strcpy(scope,"global");}

            //Scoatem "="
            if( strstr(linie, "="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Daca am gasit variabila in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Copiem tot ce e dupa linie (si are potential sa fie suprascris)
                  char rest[10000];
                  bzero(rest, 10000);
                  pread(fd, rest, 10000, lseek(fd,0,SEEK_CUR));

                  //Creem noua linie
                  lseek(fd, -inapoi, SEEK_CUR);
                  char temp[50];
                  strcpy(temp, linie);
                  snprintf(linie,100,"%s = %.2f %s",temp, valoareReal, scope);

                  //Suprascriem
                  write(fd, linie, strlen(linie));
                  write(fd,"\n",1);
                  write(fd, rest, strlen(rest));
                  return 0;
            }
      }
}
int schimbaValoareaSir(char *tip, char *numeVar, char *valoareSir)
{
      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;
      char scope[10];

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;

            //Vrem sa stim cat dam cursorul inapoi
            int inapoi = strlen(linie) + 1;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  {linie[strlen(linie) - 6] = '\0'; strcpy(scope,"local");}     
            else if(strstr(linie, "global"))  
                  {linie[strlen(linie) - 7] = '\0'; strcpy(scope,"global");}

            //Scoatem "="
            if( strstr(linie, "="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Daca am gasit variabila in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Copiem tot ce e dupa linie (si are potential sa fie suprascris)
                  char rest[10000];
                  bzero(rest, 10000);
                  pread(fd, rest, 10000, lseek(fd,0,SEEK_CUR));

                  //Creem noua linie
                  lseek(fd, -inapoi, SEEK_CUR);
                  char temp[50];
                  strcpy(temp, linie);
                  snprintf(linie,100,"%s = %s %s",temp, valoareSir, scope);

                  //Suprascriem
                  write(fd, linie, strlen(linie));
                  write(fd,"\n",1);
                  write(fd, rest, strlen(rest));
                  return 0;
            }
      }
}

int schimbaValoareaCara(char *tip, char *numeVar, char valoareCara)
{
      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;
      char scope[10];

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;

            //Vrem sa stim cat dam cursorul inapoi
            int inapoi = strlen(linie) + 1;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  {linie[strlen(linie) - 6] = '\0'; strcpy(scope,"local");}     
            else if(strstr(linie, "global"))  
                  {linie[strlen(linie) - 7] = '\0'; strcpy(scope,"global");}

            //Scoatem "="
            if( strstr(linie, "="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Daca am gasit variabila in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Copiem tot ce e dupa linie (si are potential sa fie suprascris)
                  char rest[10000];
                  bzero(rest, 10000);
                  pread(fd, rest, 10000, lseek(fd,0,SEEK_CUR));

                  //Creem noua linie
                  lseek(fd, -inapoi, SEEK_CUR);
                  char temp[50];
                  strcpy(temp, linie);
                  snprintf(linie,100,"%s = %c %s",temp, valoareCara, scope);

                  //Suprascriem
                  write(fd, linie, strlen(linie));
                  write(fd,"\n",1);
                  write(fd, rest, strlen(rest));
                  return 0;
            }
      }
}

int schimbaValoareaBoolean(char *tip, char *numeVar, char *valoareBool)
{
      //(copiat de la cautare codul)
      char linie[100]="";
      int index = 0;
      char  c;
      int gasit = 0;
      char scope[10];

      char variabila[100];
      strcpy(variabila, tip);
      strcat(variabila, " ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;

            //Vrem sa stim cat dam cursorul inapoi
            int inapoi = strlen(linie) + 1;
            
            //Remove the local/global part
            if(strstr(linie,"local"))
                  {linie[strlen(linie) - 6] = '\0'; strcpy(scope,"local");}     
            else if(strstr(linie, "global"))  
                  {linie[strlen(linie) - 7] = '\0'; strcpy(scope,"global");}

            //Scoatem "="
            if( strstr(linie, "="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Daca am gasit variabila in fisier
            if (strcmp(linie, variabila) == 0)
            {
                  //Copiem tot ce e dupa linie (si are potential sa fie suprascris)
                  char rest[10000];
                  bzero(rest, 10000);
                  pread(fd, rest, 10000, lseek(fd,0,SEEK_CUR));

                  //Creem noua linie
                  lseek(fd, -inapoi, SEEK_CUR);
                  char temp[50];
                  strcpy(temp, linie);
                  snprintf(linie,100,"%s = %s %s",temp, valoareBool, scope);

                  //Suprascriem
                  write(fd, linie, strlen(linie));
                  write(fd,"\n",1);
                  write(fd, rest, strlen(rest));
                  return 0;
            }
      }
}

int cautaVal(char *numeVar)
{
      char linie[100]="";
      int index = 0;
      char  c;
      char copie[100];

      char variabila[100];
      strcpy(variabila, "natural ");
      strcat(variabila, numeVar);      

      //Citit fisier
      lseek(fd, 0, SEEK_SET);
      while(1)
      {
            bzero(linie, 100);
            index = 0;
            //Citit o linie
            while(1)
            {
                  int r = read(fd, &c, 1);
                  if( r == 0 || c == '\n')
                        break;     
                  linie[index++] = (char )c;                   
            }

            //Am terminat de citit din fisier
            if (index == 0)
                  break;
            
            //facem o copie a liniei
            strcpy(copie, linie);

            //Remove the local/global part
            if(strstr(linie,"local"))
                  linie[strlen(linie) - 6] = '\0';      
            else if(strstr(linie, "global"))  
                  linie[strlen(linie) - 7] = '\0';   
            else linie[strlen(linie) - 2] = '\0';

            //Scoatem "="
            if(strstr(linie,"="))
            {
                  char *temp = strtok(linie, "=");
                  temp[strlen(temp) - 1] = '\0';
                  strcpy(linie, temp);
            }

            //Comparam
            //printf("comparam [%s] cu [%s] \n", linie, variabila);
            if (strcmp(linie, variabila) == 0)
            {
                  strtok(copie, "=");
                  char *valoare = strtok(NULL, " ");
                  return atoi(valoare);
            }
      }
      return 0;
}

int main(int argc, char** argv){
fd = open("simboluri.txt", O_CREAT | O_RDWR | O_TRUNC, 0666);
yyin=fopen(argv[1],"r");
yyparse();
} 
