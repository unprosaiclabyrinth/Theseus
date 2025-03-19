import java.util.Random;

class MapCell {
    public boolean visited = false;
    public boolean actualBreeze = false;
    public boolean actualStench = false;
    public char inferred = '\0';
}
//INTERNAL MAP CREATION CF. PDF DESCRIPTION
class AgentMap {
    private MapCell[][] grid;
    
    public char getCellType(MapCell cell) {
        if (cell.inferred == 'W' || cell.inferred == 'P' || cell.inferred == 'Y') {
            return cell.inferred;
        }
        if (!cell.visited) {
            return '.';
        }
        if (cell.actualBreeze && !cell.actualStench) {
            return 'b';
        }
        if (cell.actualStench && !cell.actualBreeze) {
            return 's';
        }
        if (!cell.actualBreeze && !cell.actualStench) {
            return 'V';
        }
        return 'X';
    }

    public AgentMap() {
        grid = new MapCell[4][4];
        for (int x = 0; x < 4; x++) {
            for (int y = 0; y < 4; y++) {
                grid[x][y] = new MapCell();
            }
        }
    }
    
    public MapCell getCell(int x, int y) {
        if (x >= 0 && x < 4 && y >= 0 && y < 4) {
            return grid[x][y];
        }
        return null;
    }
    //UPDATING THE MAP WITH THE PERCEPS
    public void updateMap(int x, int y, TransferPercept tp) {
        MapCell current = grid[x][y];
        current.visited = true;
        current.actualBreeze = tp.getBreeze();
        current.actualStench = tp.getStench();
        current.inferred = '\0';
        
        if (tp.getBreeze()) {
            checkDiagonalAndMark(x, y, 'b');
        }
        if (tp.getStench()) {
            checkDiagonalAndMark(x, y, 's');
        }
        
        if (tp.getScream()) {
            for (int i = 0; i < 4; i++) {
                for (int j = 0; j < 4; j++) {
                    MapCell cell = grid[i][j];
                    if (cell != null && cell.inferred == 'W') {
                        cell.inferred = '.';
                    }
                }
            }
        }
    }
    // WE CHECK IF WE CAN INFER WHERE THE W OR P ARE
    private void checkDiagonalAndMark(int x, int y, char type) {
        char hazard = (type == 'b') ? 'P' : 'W';
        int[] dx = { 1, 1, -1, -1 };
        int[] dy = { 1, -1, 1, -1 };
        
        for (int i = 0; i < 4; i++) {
            int diagX = x + dx[i];
            int diagY = y + dy[i];
            MapCell diag = getCell(diagX, diagY);
            if (diag != null) {
                if ((type == 'b' && diag.actualBreeze) || (type == 's' && diag.actualStench)) {
                    int commonX1 = x;
                    int commonY1 = y + dy[i];
                    int commonX2 = x + dx[i];
                    int commonY2 = y;
                    
                    MapCell common1 = getCell(commonX1, commonY1);
                    MapCell common2 = getCell(commonX2, commonY2);
                    
                    if (common1 != null) {
                        if (!(common1.visited && !common1.actualBreeze && !common1.actualStench)) {
                            common1.inferred = hazard;
                        }
                    }
                    if (common2 != null) {
                        if (!(common2.visited && !common2.actualBreeze && !common2.actualStench)) {
                            common2.inferred = hazard;
                        }
                    }
                }
            }
        }
    }
    //IF WE INFERRED, WE MARK SOME CELLS AS SAFE
    public void updateSafeCells() {
        for (int x = 0; x < 4; x++) {
            for (int y = 0; y < 4; y++) {
                MapCell cell = getCell(x, y);
                if (cell == null) {
                    continue;
                }
                char cellType = getCellType(cell);
                if (cell.visited && (cellType == 'b' || cellType == 's')) {
                    int[] dx = { 0, 1, 0, -1 };
                    int[] dy = { 1, 0, -1, 0 };
                    boolean sharingHazard = false;
                    
                    for (int i = 0; i < 4; i++) {
                        MapCell neighbor = getCell(x + dx[i], y + dy[i]);
                        if (neighbor == null) {
                            continue;
                        }
                        if (neighbor.inferred == 'W' || neighbor.inferred == 'P') {
                            sharingHazard = true;
                            break;
                        }
                    }
                    if (sharingHazard && (cellType == 'b' || cellType == 's')) {
                        cell.inferred = 'Y';
                    }
                }
            }
        }
    }
    
    public void printMap() {
        System.out.println("Agent Map:");
        for (int y = 3; y >= 0; y--) {
            for (int x = 0; x < 4; x++) {
                MapCell cell = grid[x][y];
                if (cell.inferred != '\0') {
                    System.out.print(cell.inferred + " ");
                } else if (cell.visited) {
                    if (cell.actualStench && cell.actualBreeze) {
                        System.out.print("X ");
                    } else if (cell.actualStench) {
                        System.out.print("s ");
                    } else if (cell.actualBreeze) {
                        System.out.print("b ");
                    } else {
                        System.out.print("V ");
                    }
                } else {
                    System.out.print(". ");
                }
            }
            System.out.println();
        }
        System.out.println();
    }
}

//
// THE AGENTFUNCTION CLASS USES THE AGENTMAP IN ITS DECISION MAKING, IT IS A IMPROVEMENT OF THE PERCEPT AS IT KEEPS ALL THE PREVIOUS PERCEPT IN IT
//

class AgentFunction {
    private String agentName = "007";
    private int[] actionTable;
    private boolean bump;
    private boolean glitter;
    private boolean breeze;
    private boolean stench;
    private boolean scream;
    private Random rand;
    private AgentMap agentMap;
    private int posX = 0;
    private int posY = 0;
    private int orientation = 1;

    public AgentFunction() {
        actionTable = new int[8];
        actionTable[0] = Action.GO_FORWARD;
        actionTable[1] = Action.GO_FORWARD;
        actionTable[2] = Action.GO_FORWARD;
        actionTable[3] = Action.GO_FORWARD;
        actionTable[4] = Action.TURN_RIGHT;
        actionTable[5] = Action.TURN_LEFT;
        actionTable[6] = Action.GRAB;
        actionTable[7] = Action.SHOOT;
        
        rand = new Random();
        agentMap = new AgentMap();
    }
    
    private char getCellType(MapCell cell) {
        if (cell.inferred == 'W' || cell.inferred == 'P' || cell.inferred == 'Y' || cell.inferred == 'p') {
            return cell.inferred;
        }
        if (!cell.visited) {
            return '.';
        }
        if (cell.actualBreeze && !cell.actualStench) {
            return 'b';
        }
        if (cell.actualStench && !cell.actualBreeze) {
            return 's';
        }
        if (!cell.actualBreeze && !cell.actualStench) {
            return 'V';
        }
        return 'X';
    }

    private boolean isInPriority(char type, char[] candidates) {
        for (char c : candidates) {
            if (type == c)
                return true;
        }
        return false;
    }
    // HERE WE DECIDE WE NEW ACTION
    private int decideAction() {
        MapCell currentCell = agentMap.getCell(posX, posY);
        // IDENTIFY SURROUNDINGS
        int nextX = posX, nextY = posY;
        switch (orientation) {
            case 0:
                nextY++;
                break;
            case 1:
                nextX++;
                break;
            case 2:
                nextY--;
                break;
            case 3:
                nextX--;
                break;
        }
        MapCell frontCell = agentMap.getCell(nextX, nextY);

        int leftX = posX, leftY = posY, rightX = posX, rightY = posY, backX = posX, backY = posY;
        switch (orientation) {
            case 0:
                leftX--;
                rightX++;
                backY--;
                break;
            case 1:
                leftY++;
                rightY--;
                backX--;
                break;
            case 2:
                leftX++;
                rightX--;
                backY++;
                break;
            case 3:
                leftY--;
                rightY++;
                backX++;
                break;
        }
        // WE DEFINE POS OF CELLS, TO USE THEM IN OUR IF STATEMENTS
        MapCell leftCell = agentMap.getCell(leftX, leftY);
        MapCell rightCell = agentMap.getCell(rightX, rightY);
        MapCell backCell = agentMap.getCell(backX, backY);

        int wCount = 0;
        for (int x = 0; x < 4; x++) {
            for (int y = 0; y < 4; y++) {
                MapCell cell = agentMap.getCell(x, y);
                if (cell != null && cell.inferred == 'W') {
                    wCount++;
                }
            }
        }

        int pCount = 0;
        for (int x = 0; x < 4; x++) {
            for (int y = 0; y < 4; y++) {
                MapCell cell = agentMap.getCell(x, y);
                if (cell != null && cell.inferred == 'P') {
                    pCount++;
                }
            }
        }

        //GRAB GOLD IMMEDIATELY IF GLITTER IS DETECTED
        if (glitter) {
            System.out.println("Rule 1");
            return Action.GRAB;
        }

        // STARTING CELL CONDITIONS
        if (!stench && !breeze && currentCell != null && getCellType(currentCell) == 'V' && frontCell != null && getCellType(frontCell) == '.') {
            System.out.println("Rule 2");
            return Action.GO_FORWARD;
        }

        if ((breeze) && frontCell != null && getCellType(frontCell) == '.' && backCell == null &&
            leftCell != null && getCellType(leftCell) == '.' &&
            rightCell == null) {
            System.out.println("Rule 3");
            return Action.TURN_LEFT;
        }

        if ((breeze) && frontCell != null && getCellType(frontCell) == '.' && backCell == null &&
            leftCell == null &&
            rightCell != null && getCellType(rightCell) == '.') {
            System.out.println("Rule 4");
            return Action.TURN_RIGHT;
        }

        if ((stench) && frontCell != null && getCellType(frontCell) == '.' && backCell == null &&
            leftCell != null && getCellType(leftCell) == '.' &&
            rightCell == null) {
            System.out.println("Rule 3X");
            return Action.SHOOT;
        }

        if ((stench) && frontCell != null && getCellType(frontCell) == '.' && backCell == null &&
            leftCell == null &&
            rightCell != null && getCellType(rightCell) == '.') {
            System.out.println("Rule 4X");
            return Action.SHOOT;
        }

        // WE KNOW PERFECTLY WHERE THE WUMPUS IS
        if (wCount == 1) {
            if (frontCell != null && getCellType(frontCell) == 'W') {
                System.out.println("Rule 5");
                return Action.SHOOT;
            }
            if (leftCell != null && getCellType(leftCell) == 'W') {
                System.out.println("Rule 6");
                return Action.TURN_LEFT;
            }
            if (rightCell != null && getCellType(rightCell) == 'W') {
                System.out.println("Rule 7");
                return Action.TURN_RIGHT;
            }
        }
        
        //DANGER
        if (currentCell != null && getCellType(currentCell) == 'X') {
            if (frontCell != null && (getCellType(frontCell) == 'Y' || getCellType(frontCell) == 'V')) {
                System.out.println("Rule 8");
                return Action.GO_FORWARD;
            }
            if (leftCell != null && (getCellType(leftCell) == 'Y' || getCellType(leftCell) == 'V')) {
                System.out.println("Rule 8.1");
                return Action.TURN_LEFT;
            }
            if (rightCell != null && (getCellType(rightCell) == 'Y' || getCellType(rightCell) == 'V')) {
                System.out.println("Rule 8.2");
                return Action.TURN_RIGHT;
            }
        }

        if (pCount == 1) {
            if (frontCell != null && getCellType(frontCell) == 'P' && leftCell != null && getCellType(leftCell) == '.') {
                System.out.println("Rule 20");
                return Action.TURN_LEFT;
            }
            if (leftCell != null && getCellType(leftCell) == 'P' && frontCell != null && getCellType(frontCell) == '.') {
                System.out.println("Rule 21");
                return Action.GO_FORWARD;
            }
            if (rightCell != null && getCellType(rightCell) == 'P' && frontCell != null && getCellType(frontCell) == '.') {
                System.out.println("Rule 22");
                return Action.GO_FORWARD;
            }
        }

        if (frontCell != null && (stench || breeze) && getCellType(frontCell) == 'Y') {
            System.out.println("Rule 11");
            return Action.GO_FORWARD;
        }
        if (leftCell != null && (stench || breeze) && getCellType(leftCell) == 'Y') {
            System.out.println("Rule 12");
            return Action.TURN_LEFT;
        }
        if (rightCell != null && (stench || breeze) && getCellType(rightCell) == 'Y') {
            System.out.println("Rule 13");
            return Action.TURN_RIGHT;
        }
        if (backCell != null && (stench || breeze) && getCellType(backCell) == 'Y') {
            System.out.println("Rule 14");
            return Action.TURN_LEFT;
        }
        if (frontCell != null && (stench || breeze) && getCellType(frontCell) == 'V') {
            System.out.println("Rule 15");
            return Action.GO_FORWARD;
        }
        if (leftCell != null && (stench || breeze) && getCellType(leftCell) == 'V') {
            System.out.println("Rule 16");
            return Action.TURN_LEFT;
        }
        if (rightCell != null && (stench || breeze) && getCellType(rightCell) == 'V') {
            System.out.println("Rule 17");
            return Action.TURN_RIGHT;
        }
        if (backCell != null && (stench || breeze) && getCellType(backCell) == 'V') {
            System.out.println("Rule 18");
            return Action.TURN_LEFT;
        }
        if ((stench || breeze) && backCell != null && getCellType(backCell) == 'V') {
            System.out.println("Rule 19");
            return Action.TURN_LEFT;
        }
        
        // RULE 12: IF ALL CELLS ARE UNEXPLORED ('.'), PREFER MOVING FORWARD OVER TURNING
        if (frontCell != null && getCellType(frontCell) == '.' &&
            leftCell != null && getCellType(leftCell) == '.' &&
            rightCell != null && getCellType(rightCell) == '.') {
            System.out.println("Rule 23");
            return Action.GO_FORWARD;
        }

        // RULE 13: IF SURROUNDED BY UNKNOWNS BUT ONE CELL IS VISITED ('V'), MOVE THERE
        if (frontCell != null && getCellType(frontCell) == '.' && getCellType(currentCell) == 'V') {
            System.out.println("Rule 24");
            return Action.GO_FORWARD;
        }
        if (leftCell != null && getCellType(leftCell) == '.' && getCellType(currentCell) == 'V') {
            System.out.println("Rule 25");
            return Action.TURN_LEFT;
        }
        if (rightCell != null && getCellType(rightCell) == '.' && getCellType(currentCell) == 'V') {
            System.out.println("Rule 26");
            return Action.TURN_RIGHT;
        }

        if (currentCell != null && getCellType(currentCell) == 'V' &&
            frontCell != null && getCellType(frontCell) == '.' &&
            leftCell != null && getCellType(leftCell) == '.') {
            System.out.println("Rule 27");
            return Action.GO_FORWARD;
        }
        
        // RULE 14: IF THE CURRENT CELL IS SAFE ('V'), BUT TWO ADJACENT CELLS ARE UNKNOWN ('.'), MOVE FORWARD
        if (currentCell != null && getCellType(currentCell) == 'V' &&
            frontCell != null && getCellType(frontCell) == '.' &&
            leftCell == null) {
            System.out.println("Rule 28");
            return Action.GO_FORWARD;
        }
        
        // WALKING IN A SAFE PATH
        if (frontCell != null && leftCell != null && leftCell == null && getCellType(frontCell) == 'V' && getCellType(leftCell) == 'V') {
            System.out.println("Rule 29");
            return Action.GO_FORWARD;
        }
        if (frontCell != null && leftCell != null && rightCell == null && getCellType(frontCell) == 'V' && getCellType(leftCell) == 'V') {
            System.out.println("Rule 30");
            return Action.GO_FORWARD;
        }
        if (frontCell != null && rightCell != null && leftCell == null && getCellType(frontCell) == 'V' && getCellType(rightCell) == 'V') {
            System.out.println("Rule 31");
            return Action.GO_FORWARD;
        }
        if (frontCell != null && leftCell != null && leftCell == null && getCellType(frontCell) == 'Y' && getCellType(leftCell) == 'Y') {
            System.out.println("Rule 32");
            return Action.GO_FORWARD;
        }
        if (frontCell != null && leftCell != null && rightCell == null && getCellType(frontCell) == 'Y' && getCellType(leftCell) == 'Y') {
            System.out.println("Rule 33");
            return Action.GO_FORWARD;
        }
        if (frontCell != null && rightCell != null && leftCell == null && getCellType(frontCell) == 'Y' && getCellType(rightCell) == 'Y') {
            System.out.println("Rule 34");
            return Action.GO_FORWARD;
        }
        
        if (frontCell == null && leftCell == null) {
            System.out.println("Rule 35");
            return Action.TURN_RIGHT;
        }
        
        // RULE 8: IF STENCH ('S') OR BREEZE ('B') IS IN FRONT BUT NOWHERE ELSE TO GO, MOVE FORWARD
        if (frontCell == null && rightCell == null) {
            System.out.println("Rule 36");
            return Action.TURN_LEFT;
        }
        
        if (leftCell != null && (stench || breeze) && getCellType(leftCell) == 'V') {
            System.out.println("Rule 37");
            return Action.TURN_LEFT;
        }
        // RULE 10: IF RIGHT CELL IS STENCH ('S') AND NO BETTER OPTIONS, TURN RIGHT
        if (rightCell != null && (stench || breeze) && getCellType(rightCell) == 'V') {
            System.out.println("Rule 38");
            return Action.TURN_RIGHT;
        }
        
        if (backCell != null && (stench || breeze) && getCellType(backCell) == 'V') {
            System.out.println("Rule 39");
            return Action.TURN_LEFT;
        }
        
        System.out.println("Rule 40");
        //IF COMPLETELY SURROUNDED BY DANGER OR UNKNOWNS, TURN RANDOMLY
        return rand.nextBoolean() ? Action.TURN_LEFT : Action.GO_FORWARD;
    }
    
    private void updatePosition(int action, boolean bump) {
        if (action == Action.GO_FORWARD && !bump) {
            switch (orientation) {
                case 0:
                    if (posY < 3)
                        posY++;
                    break;
                case 1:
                    if (posX < 3)
                        posX++;
                    break;
                case 2:
                    if (posY > 0)
                        posY--;
                    break;
                case 3:
                    if (posX > 0)
                        posX--;
                    break;
            }
        } else if (action == Action.TURN_RIGHT) {
            orientation = (orientation + 1) % 4;
        } else if (action == Action.TURN_LEFT) {
            orientation = (orientation + 3) % 4;
        }
    }
    
    public int process(TransferPercept tp) {
        bump = tp.getBump();
        glitter = tp.getGlitter();
        breeze = tp.getBreeze();
        stench = tp.getStench();
        scream = tp.getScream();
        
        agentMap.updateMap(posX, posY, tp);
        agentMap.updateSafeCells();
        
        int action = decideAction();
        updatePosition(action, bump);
        
        System.out.println("Agent position: (" + posX + ", " + posY + ") Orientation: " + orientationToString());
        agentMap.printMap();
        
        return action;
    }
    
    private String orientationToString() {
        switch (orientation) {
            case 0:
                return "North";
            case 1:
                return "East";
            case 2:
                return "South";
            case 3:
                return "West";
            default:
                return "Unknown";
        }
    }
    
    public String getAgentName() {
        return agentName;
    }
}
