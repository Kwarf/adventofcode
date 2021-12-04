mutable struct Board
	rows::Vector{Vector{Int32}}
	columns::Vector{Vector{Int32}}
end

input = readlines("input.txt")

boards = Vector{Board}()
for boardDefinition in Iterators.partition(input[3:end], 6)
	board = Board(Vector{Vector{Int32}}(), Vector{Vector{Int32}}())
	for row in boardDefinition[1:end-1]
		push!(board.rows, map((x) -> parse(Int32, x), split(strip(row), r"\s+")))
	end
	board.columns = [x[:] for x in eachrow(hcat(board.rows...))]
	push!(boards, board)
end

for number in map((x) -> parse(Int32, x), split(input[1], ","))
	for board in boards
		f(x) = x != number
		board.rows = [filter(f, x) for x in board.rows]
		board.columns = [filter(f, x) for x in board.columns]

		empty(x) = size(x, 1) == 0
		if findfirst(empty, board.rows) != nothing || findfirst(empty, board.columns) != nothing
			println("The answer to the first part is: ", sum([sum(x) for x in board.rows]) * number)
			return
		end
	end
end
