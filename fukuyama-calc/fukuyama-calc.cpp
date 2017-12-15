/*
整数計算はできた！
変数も使えるよ！
だけどマイナスなんちゃらっていうのは盲点だったね。(仕様には書いてあったけどね。)
ちょいと学べたこともあったし、これはスキルアップになったんじゃないかな。
Calc.hにまとめるとスッキリしたね。
アップロードのこと考えていなかったからフォルダで恐らく提出になるよ。
*/


#include <iostream>
#include <string>
#include "Calc.h"

using namespace std;

int main(int argc, char* argv) {
	Calc calc;
	string input;
	cout << "----------注意書きやぞ！----------" << endl << "直前の式の答えを使いたいときは変数resultに値が入ってるので使ってね" << endl << "空白には対応していないので式は詰めて書いてね" << endl << "exitかquitで終了するよ" << endl << "---------------------------------" << endl;
	int result = 0;
	while(1){
		cin >> input;
		if (input == "exit" || input == "quit")break;
		result = calc.exec(input,result);
	}
	return 0;
}

