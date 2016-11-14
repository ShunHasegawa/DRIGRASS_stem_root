# This script replace treatment codes.
# e.g. Drought -> Reduced; Pulused.drought -> reduced.frequency; Seasonal -> Summer.drought

sed -i '' 's/Drought/Reduced/g' *.R
sed -i '' 's/Pulsed.drought/Reduced.frequency/g' *.R
sed -i '' 's/Seasonl/Summer.drought/g' *.R


