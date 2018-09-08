#include <iostream>
#include <iomanip>
#include <stdlib.h>
#include <fstream>
#include <vector>
#include <map>
#include <cmath>
#include <string.h>
#include <stdio.h>
#include <sstream>
#include <algorithm>
using namespace std;

ifstream infile("thefile.txt");
int start_flag = -1;
int vec_len = 0;
ofstream outfile;
ofstream listing_file;
fstream inter_file;
int count=0;
const int sy_size=30;// size of symbol table
long int start_add_int=0;//start address
string start_add;
string temp;
int k;
string ascii_chars;
const int object_len=30;
int count_hexa = 0 ;
long int locctr_int=0; //location counter
long int prog_len = 0;
long int total_max_size;
string prog_len_hex ;
string locctr;
string line;
string obj1;
string obj2;
long int operand_int;
string label,opcode,operand,address,error;
long int start_add_flag = 0 ;
vector<string> obj_codes;
vector<int> sum;
int m_sum = 0;
vector<string> start_address;
vector<string> t_record;
vector<int> t_record_add;
map <string, string> sym_tab;
map<char, int>::iterator it;
map <string, string> optab=
{
    {"ADD","18"},
    {"AND","40"},
    {"COMP","28"},
    {"DIV","24"},
    {"J","3C"},
    {"JEQ","30"},
    {"JGT","34"},
    {"JLT","38"},
    {"JSUB","48"},
    {"LDA","00"},
    {"LDCH","50"},
    {"LDL","08"},
    {"LDX","04"},
    {"MUL","20"},
    {"OR","44"},
    {"RD","D8"},
    {"RSUB","4C"},
    {"STA","0C"},
    {"STCH","54"},
    {"STL","14"},
    {"STSW","E8"},
    {"STX","10"},
    {"SUB","1C"},
    {"TD","E0"},
    {"TIX","2C"},
    {"WD","DC"}
};
string trim(string &str)
{
    int size = str.length();
    for(int j = 0; j<=size; j++)
    {
        for(int i = 0; i <=j; i++)
        {
            if(str[i] == ' ' && str[i+1] == ' ')
            {
                str.erase(str.begin() + i);
            }
            else if(str[0]== ' ')
            {
                str.erase(str.begin());
            }
            else if(str[i] == '\0' && str[i-1]== ' ')
            {
                str.erase(str.end() - 1);
            }
        }
    }
    return str;
}
string convertHex(long int n)
{
    string numHex;
    std::stringstream stream;
    stream << std::hex << n;
    stream >> numHex;
    return numHex;
}
long int convertDec(string str)
{
    long int num;
    std::stringstream stream;
    stream << str;
    stream >> std::hex >> num;
    return num;
}
void  locctr_inc(string operand,string opcode)  //default case.
{
    if((opcode == "START")||(opcode == "start")) //checking directive start.
    {
        locctr = operand;
        locctr_int = convertDec(locctr);

        return;
    }
    locctr_int = convertDec(locctr)+3;
    locctr = convertHex(locctr_int);
}

void add_inc(string operand,string opcode)
{
    if((opcode == "START")||(opcode == "start"))//checking directive start.
    {

        start_add = operand;
        start_add_flag = convertDec(start_add);
        start_add_int =convertDec(start_add);
        return;
    }
    //if(start_flag>1)
    else
    {
        start_add_int = convertDec(start_add)+3;
        start_add = convertHex(start_add_int);
    }

}
string appending_bits(string s)
{
    string em_st = "";
    for(unsigned int i = 0; i < (6-s.size()); i++)
        em_st.append("0");
    return em_st.append(s);
}
string format(string s)
{
    //s = (s.find("0x") != string::npos || s.find("0X") != string::npos) ? s.substr(2,s.size()) : s;
    if(s.size() > 6)
    {
        s = s.substr(operand.size() - 6, s.size());
        error = "Overflow "; //return an error.
    }
    return appending_bits(s);
}
void reserve_bytes (string operand)
{
    long int b = atoi(operand.c_str());
    if (opcode == "RESW")
    {
        b*=3;
    }
    locctr_int += b;
    start_add_int += b;
    operand = convertHex(b);
}
void check_const_operand(string operand)
{
    if((operand.at(0) == 'x' || operand.at(0) == 'X'))
    {
        int count_hexa = ceil((double)((operand.substr(2,operand.size()-3)).size())/2.0);
        operand = format(operand.substr(2, operand.size()-3));
        locctr_int += count_hexa;
        start_add_int += count_hexa;
    }
    else if((operand.at(0) == 'c') || (operand.at(0) == 'C'))
    {
        int count_hexa = (operand.substr(2,operand.size()-3)).size();
        operand = format(operand.substr(2, operand.size()-3));
        locctr_int += count_hexa;
        start_add_int += count_hexa;

    }
    else //it's a decimal number.
    {
        reserve_bytes(operand);

    }
    start_add = convertHex(start_add_int);
    locctr = convertHex(locctr_int);
}

int check_directive(string opcode)
{
    if(opcode =="BYTE")
    {
        check_const_operand(operand);
        return 1;
    }
    else if(opcode =="WORD")
    {
        locctr_inc(operand,opcode) ;
        add_inc(operand,opcode);
        return 1;
    }
    else if(opcode =="RESB")
    {
        reserve_bytes(operand);
        start_add = convertHex(start_add_int);
        locctr = convertHex(locctr_int);
        return 1;
    }
    else if(opcode =="RESW")
    {
        reserve_bytes(operand);
        start_add = convertHex(start_add_int);
        locctr = convertHex(locctr_int);
        return 1;
    }
    else
    {
        return -1;
    }

}
int  search_label1(string label)//search for symbol and insert it if it is not found(pass1)
{
    for (auto const& x : sym_tab)
    {
        // int comp_result;
        if (x.first == label)
            return 0;
        /**std::cout << x.first  // string (key)
                  << ':'
                  << x.second // string's value
                  << std::endl ;**/
    }
    return 1;
}
int check_opcode(string opcode)
{
    for (auto const& x : optab)
    {
        // int comp_result;
        if (x.first == opcode)
            return 1; //the memonic exists and valid.

    }
    return 0; //invalid opcode.

}

int  check_indexing( string operand)
{
    int op_size=operand.size();
    int i=0;
    while(i<op_size)
    {
        if(operand[i]!=',')
            i++;
        else
        {
            if(operand[i+1]=='X')
            {
                //operand[i]=NULL;
                return 1; //indexed.
            }
        }
    }
    return 0;
}

void H_record_sum()
{
    vec_len=0;
    m_sum =0;
    string t="";
    t_record_add.push_back(0);
    for (int j = 0 ; j < k ; j++)
    {
        if(((obj_codes[j].length())+m_sum <= 60)&&(obj_codes[j]!="**"))
        {
            m_sum =  (obj_codes[j].length())+m_sum;
            t+=obj_codes[j];

        }
        else
        {
            sum.push_back(m_sum);
            t_record.push_back(t);

            t_record_add.push_back(j);
            vec_len++;
            m_sum=0;
            t="";

        }
    }

}
void H_record ()
{
    H_record_sum();
    for(int i = 0 ; i<vec_len; i++)
    {
        if(sum[i]!=0)
            outfile<<"T "<<appending_bits(start_address[t_record_add[i]])<<" "<<convertHex(ceil(t_record[i].length()/2))<<" "<<t_record[i]<<endl;
    }

}
int main()
{
    inter_file.open ("intFile.txt");
    outfile.open("output.txt");
    listing_file.open("list.txt");
    int directive_checker = 0;
    int opcode_checker = -1;
    int duplicate_flag = -1;
    int s = 0;
    string line;

    while (getline(infile, line)) //pass 1.
    {
        start_flag++;
        operand="\t";
        label="\t";
        opcode = "\t";
        istringstream iss(line);
        string token;
        int j = 0;
        while (getline(iss, token, '\t'))
        {

            if(j==0 && token != "\t" )
                label= token;
            else if (j==1 && token != "\t"  )
                opcode =token;
            else if(j==2 &&  token != "\t" )
                operand =token;
            j++;

        }
        if(operand =="\t" && label=="\t" && opcode=="\t")
            continue;
        //cout<<label << " f "<<opcode<<" f "<<operand<<endl;

        if(label =="." || label[0] =='.')
        {

            continue;
        }
        duplicate_flag = search_label1(label);
        opcode_checker = check_opcode(opcode);
        /**if (opcode_checker!= 1)
        {
            //throw an error of invalid opcode.
            continue;
        }**/

        if ((duplicate_flag == 1)&&(label != "\t")&&(label !=""))
            sym_tab[label] = start_add;
        else
        {
            //throw an error of duplicate label.
            if((duplicate_flag != 1)&&(label == "\t"))  //bugged as hell!
            {
                cout<<"I'm outta of here  "<<duplicate_flag<<" label "<<label<<endl;
                continue;
            }
        }

        directive_checker = check_directive(opcode);
        if(directive_checker == -1)
        {
            locctr_inc(operand,opcode);
            add_inc(operand,opcode);

        }
        start_address.push_back(start_add);
        if((s>0) && (opcode != "END"))
            inter_file << start_address[s-1] << "\t"<<label <<"\t"<< opcode <<"\t"<<operand;
        else if ((s>0))
            inter_file <<  "\t"<<label <<"\t"<< opcode <<"\t"<<operand;
        else
            inter_file << start_address[s] << "\t"<<label <<"\t"<< opcode <<"\t"<<operand;

        if(opcode == "END")
        {

            prog_len = convertDec(start_address[s-1]) - start_add_flag;
            prog_len_hex = convertHex(prog_len);
            cout<<" Program length : "<<prog_len_hex<<endl;

            break;
        }
        inter_file<<endl;

        s++;
    }
    inter_file.close();
    infile.close();
    inter_file.open ("intFile.txt");
    string loc;
    k = 0;
    for(auto it = sym_tab.cbegin(); it != sym_tab.cend(); ++it)
    {

        std::cout << it->first << "\t" << it->second <<  "\n";
    }
    while (getline(inter_file, line)) //pass 2.
    {
        operand="\t";
        label="\t";
        opcode = "\t";
        istringstream iss(line);
        string token;
        int j = 0;
        while (getline(iss, token, '\t'))

        {
            if(j ==0 )
                loc = token;
            else if(j==1 && token != "\t" )
                label= token;
            else if (j==2 &&  token != "\t" )
                opcode =token;
            else if(j==3 &&  token != "\t")
                operand =token;
            j++;

        }
        //cout << loc<< "\t"<<label <<"\t"<< opcode <<"\t"<<operand<<endl;
        if(operand !="\t")
            operand = trim(operand);
        if(opcode !="\t")
            opcode = trim(opcode);
        if(label !="\t")
            label = trim(label);
        loc = trim(loc);
        if(opcode=="END")
            break;
        if(opcode == "START")
        {
            outfile<<'H'<<" "<<label<<" "<<format(operand)<<" "<<format(prog_len_hex)<<endl;
            listing_file << loc<< "\t"<<label <<"\t"<< opcode <<"\t"<<operand;
            listing_file <<endl;
            continue;
        }
        else
        {
            if((opcode=="RESB") || (opcode=="RESW"))
            {
                obj_codes.push_back("**");
                listing_file << loc<< "\t"<<label <<"\t"<< opcode <<"\t"<<operand<<endl;
                k++; //to increment vector.
                continue;
            }
            if(operand== "\t" )
            {
                temp = obj1;
            }

            if((opcode!="WORD") && (opcode!="BYTE"))
            {
                obj1 = optab.find(opcode)->second;
            }
            if((opcode=="WORD") || (opcode=="BYTE"))
            {

                if((operand.at(0) == 'c') || (operand.at(0) == 'C'))
                {
                    // cout<<"It's a c "<<endl;

                    int p;
                    string p_string;
                    for(unsigned int i=2 ; i<operand.size()-1; i++)
                    {
                        p = int(operand[i]);
                        p_string = convertHex(p);
                        ascii_chars += p_string;
                    }

                }
                else if((operand.at(0) == 'x' || operand.at(0) == 'X'))
                {
                    // cout<<"It's an x "<<endl;
                    string tmp =operand.substr(2,operand.size()-3);
                    ascii_chars =  (tmp);
                    //cout<<" ascii "<<ascii_chars<<endl;
                }
                else
                {
                    ascii_chars = format(convertHex(atoi(operand.c_str())));
                }
                temp= ascii_chars;
            }

            else
            {
                int index_flag = check_indexing(operand);

                if((operand=="\t")||(operand ==""))
                {

                    obj2="0000";
                }

                else
                {
                    obj2 =sym_tab[operand];


                }
                temp = obj1+obj2;

                if(index_flag)
                {
                    obj2 =sym_tab[operand.substr(0,operand.size()-2)];
                    cout<<" obj 1 "<<obj1<<" temp " <<temp<<" obj 2 "<<obj2<<endl;
                    temp+=obj2;
                    cout<<" obj 1 "<<obj1<<" temp " <<temp<<" obj 2 "<<obj2<<" OPERAND " <<operand<<endl;
                    temp = convertHex(convertDec(temp)+convertDec("8000"));
                }
                //cout<<"Check Point 7"<<endl;

            }
            obj_codes.push_back(temp);
            //cout<<"Check Point 8"<<endl;
            listing_file << loc<< "\t"<<label <<"\t"<< opcode <<"\t"<<operand<<"\t"<<obj_codes[k]<<endl;
            k++;
        }
    }

    H_record();
    outfile<<'E'<<" "<<appending_bits(start_address[t_record_add[0]])<<endl;

    return 0;
}
