//This Java code was automatically created by tessla-compiler from a TeSSLa Specification
import java.io.BufferedReader;
import java.io.InputStreamReader;

public class Main {

    private static void outputVar(String output, String trueName, long errorCode, long ts) {
        String outputWithError = (errorCode != 0) ? "FATAL: " + trueName + " evaluation encountered an Error: " : "";
        switch((int)errorCode) {
            case 0:
                outputWithError = output;
                break;
            case 4:
                outputWithError += "Division by zero";
                break;
            case 8:
                outputWithError += "Map access to non existing key";
                break;
            default:
                outputWithError += "Unknown error code " + errorCode;
        }
        System.out.println(ts + ": " + outputWithError);
        if (errorCode != 0) System.exit((int)errorCode);
    }

    public static void main(String[] args) {

        long lastProcessedTs = 0;
//VARDEF

        try {
            long newInputTs = 0;
            String inputStream = "";
            String value = "";

            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));

            String line;
            boolean inputEndReached = false;

            while (true) {

                if ((line = br.readLine()) != null) {
                    if (line.startsWith("$timeunit")) {
                        System.out.println(line);
                        continue;
                    }
                    newInputTs = java.lang.Long.parseLong(line.split(":")[0].trim());
                    String rightPart = line.split(":")[1];
                    inputStream = rightPart.split("=")[0].trim();
                    if (rightPart.contains("=")) {
                        value = rightPart.split("=")[1].trim();
                    } else {
                        value = "";
                    }
                } else {
                    inputEndReached = true;
                }

                if (newInputTs > currTs || inputEndReached) {
                    if (inputEndReached) {
                        newInputTs++;
                    }

                    while (true) {

//TRIGGER

                        if (currTs == newInputTs) break;

//STEP

                        lastProcessedTs = currTs;
                        currTs = newInputTs;
                    }
                    if (inputEndReached) break;
                } else if (newInputTs < currTs) {
                    System.err.println(currTs + ": FATAL: decreasing timestamp received");
                    System.exit(1);
                }
//INPUTPROCESSING
            }
        } catch (Exception e) {
            System.err.println("ERROR parsing the input Stream\n\n" + e);
            System.exit(2);
        }
    }
}
