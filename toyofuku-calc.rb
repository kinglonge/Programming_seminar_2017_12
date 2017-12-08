
# 2017.12プログラミングゼミ
# 変数電卓(CLI)
# コーディング: M2豊福
# 言語: Ruby
# 参考にしたもの：「たのしいRuby第3版]「スクリプトエンジンプログラミング p.2 ~ p.80」

module Calc
    TokenError = Class.new(StandardError)               # トークンエラー
    SyntaxError = Class.new(StandardError)              # 構文エラー
    UndefinedIdentifierError = Class.new(StandardError) # 未定義の変数参照エラー
    
    CriticalError = Class.new(StandardError)            # 内部エラー

    HELP_EXOP_MESSAGE = <<-EOB
    $quit
    プログラムを終了します.
    
    $show
    定義済みの変数とその値を表示します.
    
    $undef -v -[変数名]
    指定した変数を未定義にします.
    
    $undef -a 
    定義済みの全ての変数を未定義にします.
    EOB

    # Calc::TokenId モジュール
    #   トークンの種別
    #   0以上 : 式のトークン
    #   0未満 : システム命令トークン
    module TokenId
        EXPR_END = 0    # 式の終わりを識別するためのトークン
        OP_CONST = 1    # 定数（数値）
        OP_VALUE = 2    # 変数
        OP_POS = 8      # +:正の符号
        OP_NEG = 9      # -:負の符号
        OP_ADD = 10     # +:加算演算子
        OP_SUB = 11     # -:減算演算子
        OP_MUL = 12     # *:乗算演算子
        OP_DIV = 13     # /:除算演算子
        OP_MOD = 14     # %:剰余演算子
        OP_ASN = 15     # =:代入演算子
        LEFT_PER = 15   # (:開始カッコ
        RIGHT_PER = 16  # ):閉じカッコ

        EXOP_HELP = -1      # ヘルプを表示する
        EXOP_QUIT = -2      # 終了する
        EXOP_SHOW = -3      # 定義済みの変数を表示する
        EXOP_UNDEF = -4     # 指定した変数を未定義にする
        EXOP_UNDEF_ALL = -5 # 変数をすべて未定義にする
    end
    
    # Calc::Tokenクラス
    #   文字列として与えられた式を分割したもの
    class Token
        attr_reader :id_, :value_
        def initialize(id, value)
            @id_ = id       # TokenIdのモジュール変数
            @value_ = value # トークンの文字列
        end
    end

    # Calc.ValueTable クラス
    #   変数テーブル
    class ValueTable
        attr_accessor :table_

        def initialize
            @table_ = Hash.new
        end

        # ValueTable.[] 関数
        #   指定した変数の値を返す
        def [](name)
            if define?(name)
                return @table_[name]
            else
                raise UndefinedIdentifierError, "未定義の変数 '#{name}' を参照しようとしました."
            end
        end

        # ValueTable.[]= 関数
        #   変数を定義する
        def []=(name, value)
            return @table_[name] = value
        end

        # ValueTable.define? 関数
        #   指定した変数が定義されていればtrue, それ以外はfalseを返す
        def define?(name)
            return @table_.has_key?(name)
        end

        # ValueTable.empty? 関数
        #   変数がひとつも定義されていなければtrue, それ以外はfalseを返す
        def empty?
            return @table_.empty?
        end

        # ValueTable.undef 関数
        #   指定した変数を未定義にする
        def undef(name)
            if define?(name)
                @table_.delete(name)
            else
                raise UndefinedIdentifierError, "未定義の変数 '#{name}' を削除しようとしました."
            end
        end

        # ValueTable.undef_all 関数
        #   変数をすべて未定義にする
        def undef_all
            @table_.clear()
        end

        # ValueTable.get_info 関数
        #   定義済みの変数名とその値を文字列として返す
        def get_info
            info = ""
            if empty?
                info = "# 変数は何も定義されていません.\n"
            else
                @table_.each do|name, value|
                    info << name << " : " << value.to_s << "\n"
                end
            end
            return info
        end
    end

    # Calc::Nodeクラス
    #   構文木のノード
    class Node
        attr_reader :id_, :left_, :right_, :value_

        def initialize(id, left, right, value)
            @id_ = id           # ノードの種類. TokenIdのどれか.
            @left_ = left       # 左ノード
            @right_ = right     # 右ノード
            @value_ = value     # ノードの値. OP_CONSTなら数値文字列. OP_VALUEなら変数名文字列. それ以外は0.
        end

        # Node.expr関数
        #   ノードの左辺と右辺で演算したり, 定数や変数の参照, 代入処理を行う
        #   引数はCalc::TalueTableクラスのオブジェクト
        def expr(value_table)
            if !value_table.instance_of?(ValueTable)
                raise CriticalError, "Node.expr関数にはValueTableオブジェクトを与えてください."
            end

            if @id_ == TokenId::OP_ADD
                return @left_.expr(value_table) + @right_.expr(value_table)

            elsif @id_ == TokenId::OP_SUB
                return @left_.expr(value_table) - @right_.expr(value_table)

            elsif @id_ == TokenId::OP_MUL
                return @left_.expr(value_table) * @right_.expr(value_table)

            elsif @id_ == TokenId::OP_DIV
                return @left_.expr(value_table) / @right_.expr(value_table)

            elsif @id_ == TokenId::OP_MOD
                return @left_.expr(value_table) % @right_.expr(value_table)

            elsif @id_ == TokenId::OP_CONST
                return @value_.to_f()

            elsif @id_ == TokenId::OP_VALUE
                return value_table[@value_]

            elsif @id_ == TokenId::OP_POS
                return @left_.expr(value_table)

            elsif @id_ == TokenId::OP_NEG
                return @left_.expr(value_table) * (-1)

            elsif @id_ == TokenId::OP_ASN
                value_table[@left_.value_] = @right_.expr(value_table)
                return value_table[@left_.value_]

            else
                raise SyntaxError, "不明なトークンIDです."
            end
        end
    end

    # Calc::Tokenizerクラス
    #   式として与えられた文字列をトークンに分割するためのクラス.
    class Tokenizer
        attr_reader :expr_

        @@CONST_NUM_REGEX = /(\d+(?:\.\d+)*)/ # 定数の正規表現
        @@VALUE_REGEX = /([a-zA-Z_][\w]*)/    # 変数の正規表現

        @@EX_QUIT_REGEX = /(\$quit)/                      # EXOP_QUIT
        @@EX_SHOW_REGEX = /(\$show)/                      # EXOP_SHOW
        @@EX_UNDEF_REGEX = /\$undef\-v\-([a-zA-Z_][\w]*)/ # EXOP_UNDEF
        @@EX_UNDEF_ALL_REGEX = /(\$undef\-a)/             # EXOP_UNDEF_ALL

        def initialize(expr)
            @expr_ = expr.gsub(/\s+/, "") # 与式文字列から空白を除去したものを式とする
        end

        # Tokenizer.get_next_token 関数
        #   与式の先頭からトークンを取り出して返す
        #   取り出せるトークンがない場合はEXPR_ENDトークンを返す
        #   トークンが識別できない場合は例外を投げる
        #   取り出したトークン文字列は@expr_から削除される
        def get_next_token
            # 先頭トークンの種類を取得
            token_id, token_value, token_length = check_next_token()

            # 与えられた式から取り出したトークン部分を削除
            @expr_.slice!(0...token_length)

            # トークンを生成
            token = Token.new(token_id, token_value)

            return token
        end

        # Tokenizer.check_next_token 関数
        #   式の最初の１文字から先頭トークンの種類を判定する.
        #   返り値はトークンID, トークンの値, トークンの長さ.
        def check_next_token
            # 最初の文字を取得
            first_char = @expr_.chr
            
            # これ以上トークンが取り出せない場合はEXPR_ENDと判定する
            if first_char == ""
                return TokenId::EXPR_END, "", 0
            end

            # トークンの種類判定
            if first_char == "+"
                # 加算演算子
                return TokenId::OP_ADD, "+", 1
                
            elsif first_char == "-"
                # 減算演算子 or 負の符号
                return TokenId::OP_SUB, "-", 1

            elsif first_char == "*"
                # 乗算演算子
                return TokenId::OP_MUL, "*", 1

            elsif first_char == "/"
                # 除算演算子
                return TokenId::OP_DIV, "/", 1

            elsif first_char == "%"
                # 剰余演算子
                return TokenId::OP_MOD, "%", 1

            elsif first_char == "("
                # 丸かっこ開始
                return TokenId::LEFT_PER, "(", 1

            elsif first_char == ")"
                # 丸かっこ閉じ
                return TokenId::RIGHT_PER, ")", 1

            elsif first_char == "="
                # 代入演算子
                return TokenId::OP_ASN, "=", 1

            elsif /\d/ =~ first_char
                # 数値
                @@CONST_NUM_REGEX  =~ @expr_
                return TokenId::OP_CONST, $1, $1.length

            elsif /([a-zA-Z_])/ =~ first_char
                # 変数
                @@VALUE_REGEX =~ @expr_
                return TokenId::OP_VALUE, $1, $1.length
                
            elsif /(\$)/ =~ first_char
                # システム命令
                if @@EX_QUIT_REGEX =~ @expr_
                    # 停止
                    return TokenId::EXOP_QUIT, $1, $1.length

                elsif @@EX_SHOW_REGEX =~ @expr_
                    # 定義済みの変数表示
                    return TokenId::EXOP_SHOW, $1, $1.length

                elsif @@EX_UNDEF_REGEX =~ @expr_
                    # 指定した変数を未定義にする
                    return TokenId::EXOP_UNDEF, $1, $1.length

                elsif @@EX_UNDEF_ALL_REGEX =~ @expr_
                    # 変数をすべて未定義にする
                    return TokenId::EXOP_UNDEF_ALL, $1, $1.length

                else
                    # 不明なシステム命令の場合, Helpを表示する命令とする.
                    return TokenId::EXOP_HELP, HELP_EXOP_MESSAGE, 1
                end
            else
                # トークンが識別できない場合、例外を投げる
                raise TokenError, "不明なトークンです. '#{first_char}'"
            end
        end

        # Tokenizer.get_token_array 関数
        #   トークン列を返す. 最後のトークンはEXPR_ENDとなる.
        def get_token_array
            token_array = Array.new
            loop do 
                token = get_next_token()
                token_array.push(token)
                if token.id_ == TokenId::EXPR_END
                    break
                end
            end
            return token_array
        end
    end
    
    # Calc::Parser クラス
    #   構文木を作るためのクラス
    class Parser
        def initialize
            @token_array_ = nil
        end

        # Parser.parse 関数
        #   与式をパースして構文木を作る
        #   システム命令トークンを最初に読み込んだ場合, 構文木は作られずに命令ノードだけ返す
        def parse(expr)
            tokenizer = Tokenizer.new(expr)
            @token_array_ = tokenizer.get_token_array()

            token = @token_array_.shift()

            # 空行または, システム命令なら該当ノードを返す
            if token.id_ <= 0
                return Node.new(token.id_, nil, nil, token.value_)
            end

            # トークンを元に戻す
            @token_array_.unshift(token)

            # 代入処理
            root = assign_expr()
            return root
        end

        private
        
        # Parser.assign_expr関数
        #   代入処理
        def assign_expr
            first_token = @token_array_.shift()
            second_token = @token_array_.shift()

            # [変数] = の形になっているかどうか判定
            if first_token.id_ == TokenId::OP_VALUE && second_token.id_ == TokenId::OP_ASN
                # 変数定義ノード
                left = Node.new(TokenId::OP_VALUE, nil, nil, first_token.value_)
                right = assign_expr()
                return Node.new(TokenId::OP_ASN, left, right, 0)

            else
                # これ以上代入処理はないので, トークンを元に戻して加減算処理
                @token_array_.unshift(second_token)
                @token_array_.unshift(first_token)
                return add_sub_expr()
            end
        end

        # Parse.add_sub_expr 関数
        #   加減算の処理
        def add_sub_expr
            # 左辺の計算
            left = mul_div_expr()

            loop do
                # 右辺の計算
                op_token = @token_array_.shift()
                op_token_id = op_token.id_

                # トークンが加算、減算演算子かどうか判定
                if op_token_id!= TokenId::OP_ADD && op_token_id != TokenId::OP_SUB

                    # 閉じカッコか与式の終わりトークンでなければ, 不正なトークン列と判断.
                    if op_token_id != TokenId::EXPR_END && op_token_id != TokenId::RIGHT_PER
                        raise SyntaxError, "不正なトークン列です. '#{op_token.value_}'"
                    end
                    
                    # これ以上, 足すものがないので足し算終わり
                    @token_array_.unshift(op_token)
                    break
                end
                
                right = mul_div_expr()
                
                # 作成したノードを左辺にして繰り返す.
                left = Node.new(op_token_id, left, right, 0)
            end
            return left
        end

        # Parse.mul_div_expr 関数
        #   乗除算の処理
        def mul_div_expr
            # 左辺の計算
            left = term()

            loop do
                # 右辺の計算
                op_token = @token_array_.shift()
                op_token_id = op_token.id_

                # トークンが乗算、除算演算子かどうか判定
                if op_token_id!= TokenId::OP_MUL \
                    && op_token_id != TokenId::OP_DIV \
                    && op_token_id != TokenId::OP_MOD

                    # 閉じカッコ, 与式の終わり, 加算, 減算トークンでなければ, 不正なトークン列と判断.
                    if op_token_id != TokenId::EXPR_END \
                        && op_token_id != TokenId::RIGHT_PER \
                        && op_token_id != TokenId::OP_ADD \
                        && op_token_id != TokenId::OP_SUB
                        raise SyntaxError, "不正なトークン列です. '#{op_token.value_}'"
                    end

                    # これ以上, かけるものがないので, 掛け算終わり
                    @token_array_.unshift(op_token)
                    break
                end

                right = term()

                # 作成したノードを左辺にして繰り返す.
                left = Node.new(op_token_id, left, right, 0)
            end
            return left
        end

        # Parse.term 関数
        #   終端記号の処理
        def term
            token = @token_array_.shift()
            token_id = token.id_

            if token_id == TokenId::OP_ADD
                # 正の符号
                return Node.new(TokenId::OP_POS, term(), nil, 0)

            elsif token_id == TokenId::OP_SUB
                # 負の符号
                return Node.new(TokenId::OP_NEG, term(), nil, 0)

            elsif token_id == TokenId::LEFT_PER
                # ()でくくられた数式

                # ()の中身を計算
                expr_node = add_sub_expr()

                # 閉じカッコがあるかどうか判定
                token = @token_array_.shift()
                if token.id_ != TokenId::RIGHT_PER
                    raise SyntaxError, "閉じカッコ ')' がありません."
                end
                return expr_node

            elsif token_id == TokenId::OP_CONST
                # 定数（数値）
                return Node.new(TokenId::OP_CONST, nil, nil, token.value_)

            elsif token_id == TokenId::OP_VALUE
                # 変数
                return Node.new(TokenId::OP_VALUE, nil, nil, token.value_)
            else
                # 識別不能
                raise SyntaxError, "不明なトークンが検出されたか, トークン列が不正です. '#{token.value_}'"
            end
        end
    end
end

if __FILE__ == $0
    value_table = Calc::ValueTable.new   # 変数テーブル
    parser = Calc::Parser.new            # パーサオブジェクト
    
    print Calc::HELP_EXOP_MESSAGE
    print ">> "
    
    while expr = gets
        begin
            # パース
            root = parser.parse(expr)
    
            # システムメッセージなら処理, 空行なら飛ばす
            node_id = root.id_
            if node_id <= 0
                if node_id == Calc::TokenId::EXOP_QUIT
                    print "終了"
                    break
        
                elsif node_id == Calc::TokenId::EXOP_SHOW
                    print value_table.get_info()
        
                elsif node_id == Calc::TokenId::EXOP_UNDEF
                    value_table.undef(root.value_)
        
                elsif node_id == Calc::TokenId::EXOP_UNDEF_ALL
                    value_table.undef_all()
    
                elsif node_id == Calc::TokenId::EXOP_HELP
                    print root.value_
                end
                next
            end
    
            # 計算
            ret = root.expr(value_table)
            
            # 結果を表示
            print ret, "\n"

        rescue => ex
            print "エラー : ", ex.message, "\n"
        ensure
            print ">> "
        end
    end
end
