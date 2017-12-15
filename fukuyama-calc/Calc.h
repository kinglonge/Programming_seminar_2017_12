#pragma once
#include <iostream>
#include <string>
#include <vector>
#include <regex>
#include <map>

using namespace std;

class Calc{
public:
	int exec(string input,int result) {
		saveOperator(input);
		saveItem(input);
		result = calc();
		clearVec();
		return result;
	}

private:
	vector<int> ope;	//オペレータの位置を保存
	vector<string> vec;	//文字列及びオペレータを保存
	map<string, int>value;//連想配列で変数に値を格納

	//演算子に分解
	void saveOperator(string word) {
		int preOpeNum = -1;	//一つ前のオペレータの位置を保存
		int braCheck = 0;	//括弧の数を保存
		//bool itemCheck = false;

		//オペレータの位置をopeに保存
		for (int i = 0; i < (int)word.size(); i++) {
			//演算子を読み込んだ時
			if (word[i] == '=' || word[i] == '+' || word[i] == '-' || word[i] == '*' || word[i] == '/' || word[i] == '%') {
				ope.push_back(i);
				//連続で演算子が並んでいた場合はエラーを返す
				if (preOpeNum == i - 1) {
					cout << "構文がなんかおかしいよ！" << endl;
					exit(1);
				}
				preOpeNum = i;
				/*if (itemCheck == false) {
					itemCheck = true;
				}*/
			}
			//括弧を読み込んだ時
			else if (word[i] == '(') {
				ope.push_back(i);
				braCheck++;
				if (preOpeNum != i - 1) {
					cout << "括弧の前は演算子じゃないとだめだよ！" << endl;
					exit(1);
				}
				preOpeNum = i;
				/*if (itemCheck == false) {
					itemCheck = true;
				}*/
			}
			//閉じ括弧を読み込んだ時
			else if (word[i] == ')') {
				ope.push_back(i);
				if (braCheck <= 0) {
					cout << "括弧なんかがおかしいよ！" << endl;
					exit(1);
				}
				else if (preOpeNum == i - 1) {
					cout << "構文がなんかおかしいよ！" << endl;
					exit(1);
				}
				braCheck--;
			}
		}
		//ループ終了時に括弧の数を確認
		if (braCheck != 0) {
			cout << "括弧がなんかおかしいよ！" << endl;
			exit(1);
		}
		//項しかない場合
		//if (!itemCheck) {
			//cout << word << endl;
			//exit(0);
		//}
	}

	//項とオペレータをそれぞれvecに保存する
	void saveItem(string word) {
		if (!(ope.empty())) {
			int count = 0;
			int i = 0;
			for (i = 0; i < ope.size(); i++) {
				if (word[ope[i]] != '(' || word[ope[i]] != ')') {
					if (word.substr(count, ope[i] - count) != "") {
						vec.push_back(word.substr(count, ope[i] - count));
					}
					vec.push_back(word.substr(ope[i], 1));
					count = ope[i] + 1;
				}
				else {
					vec.push_back(word.substr(ope[i], 1));
					count++;
				}
			}
			if (vec[vec.size() - 1] != ")") {
				vec.push_back(word.substr(ope[i - 1] + 1, word.size() - ope[i - 1]));
			}
		}
		else {
			vec.push_back(word);
		}
	}

	//計算
	int calc() {
		int priori;
		int prioriRank;
		regex re("[0-9]+");
		while (1) {
			priori = -1;
			prioriRank = -1;
			for (int i = 0; i < vec.size(); i++) {
				if (vec[i] == "(") {
					prioriRank = 0;
					priori = i;
				}
				else if ((vec[i] == "*" || vec[i] == "/" || vec[i] == "%") && prioriRank <2) {
					priori = i;
					prioriRank = 2;
				}
				else if ((vec[i] == "+" || vec[i] == "-") && prioriRank <1) {
					priori = i;
					prioriRank = 1;
				}
				else if (vec[i] == ")" && prioriRank == 0) {
					prioriRank = 3;
					break;
				}
				else if (vec[i] == ")") {
					break;
				}
				else if (vec[i] == "=") {
					if (i != 1 || regex_match(vec[0],re)) {
						cout << "構文がおかしいよ！" << endl;
					}
					priori = i;
					prioriRank = -2;
				}
			}

			//項しかない時には終了
			if (priori == -1) {
				break;
			}

			//演算子が残っている場合
			if (prioriRank == 1 || prioriRank == 2) {

				string a = vec[priori - 1];
				string b = vec[priori + 1];
				int x = 0;
				int y = 0;

				//a変数じゃない場合
				if (regex_match(a,re)) {
					x = stoi(a);
				}
				//a変数だった場合(正規表現)
				else {
					if (!value[a]) {
						cout << "そんな変数定義してないでしょ！" << endl;
						exit(1);
					}
					x = value[a];
				}
				//b変数じゃない場合
				if (regex_match(b,re)) {
					y = stoi(b);
				}
				//b変数だった場合(正規表現)
				else {
					if (!value[b]) {
						cout << "そんな変数定義してないでしょ！" << endl;
						exit(1);
					}
					y = value[b];
				}

				
				if (vec[priori] == "+") {

					vec[priori] = to_string(x + y);
				}
				else if (vec[priori] == "-") {

					vec[priori] = to_string(x - y);
				}
				else if (vec[priori] == "*") {

					vec[priori] = to_string(x * y);
				}
				else if (vec[priori] == "/") {

					if (y == 0) {
						cout << "0で割ることはできません！反省してください！" << endl;
						exit(1);
					}
					vec[priori] = to_string(x / y);
				}
				else if (vec[priori] == "%") {

					if (y == 0) {
						cout << "0で割ることはできません！反省してください！" << endl;
						exit(1);
					}
					vec[priori] = to_string(x % y);
				}
				vec.erase(vec.begin() + priori + 1);
				vec.erase(vec.begin() + priori - 1);
			}

			if (prioriRank == 3) {
				vec.erase(vec.begin() + priori + 2);
				vec.erase(vec.begin() + priori);
			}

			if (prioriRank == -2) {
				//代入するものが数値なら
				if (regex_match(vec[2], re)) {
					value[vec[0]] = stoi(vec[2]);
				}
				//変数の値を代入するなら
				else {
					value[vec[0]] = value[vec[2]];
				}
				vec.erase(vec.begin() + 1);
				vec.erase(vec.begin());
			}
		}
		if (regex_match(vec[0],re)) {
			cout << stoi(vec[0]) << endl;
			value["result"] = stoi(vec[0]);
			return stoi(vec[0]);
		}
		else {
			cout << value[vec[0]] << endl;
			value["result"] = value[vec[0]];
			return value[vec[0]];
		}
	}

	//vec,ope(直前の計算式)の内容を消す
	void clearVec() {
		ope.clear();
		vec.clear();
	}
};
