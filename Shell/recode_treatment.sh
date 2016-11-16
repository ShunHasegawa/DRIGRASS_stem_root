# This script replace treatment codes.
# e.g. Drought -> Reduced; Pulused.drought -> reduced.frequency; Seasonal -> Summer.drought

sed -i '' 's/Drought/Reduced/g' *.R
sed -i '' 's/Pulsed.drought/Reduced.frequency/g' *.R
sed -i '' 's/Seasonal/Summer.drought/g' *.R


