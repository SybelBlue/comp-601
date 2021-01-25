part_count, purchased = map(int, input().split())

parts = set()
end_message = "paradox avoided"

for i in range(purchased):
    parts.add(input())
    if len(parts) == part_count:
        end_message = i + 1
        break

for _ in range(i + 1, purchased):
    input()

print(end_message)
